#!/bin/bash

# psyop.ca Server Deployment Script
# Deploys the Haskell website to a remote server using Git

set -e

# Configuration
SERVER_HOST="157.245.87.253"
SERVER_USER="root"
DEPLOY_PATH="/var/www/psyop.ca"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."
    
    # Check if git repo is clean
    if ! git diff --quiet; then
        error "You have uncommitted changes. Please commit or stash them first."
    fi
    
    # Check if we can reach the server
    log "Testing SSH connectivity to ${SERVER_HOST}..."
    echo "Note: You'll need to enter the server password when prompted"
    
    success "Prerequisites check completed"
}

# Setup server environment
setup_server() {
    log "Setting up server environment..."
    
    ssh "${SERVER_USER}@${SERVER_HOST}" << 'EOF'
        # Update system packages
        apt update && apt upgrade -y
        
        # Install required packages
        apt install -y git curl build-essential nginx certbot python3-certbot-nginx
        
        # Install Docker
        if ! command -v docker &> /dev/null; then
            curl -fsSL https://get.docker.com -o get-docker.sh
            sh get-docker.sh
            systemctl enable docker
            systemctl start docker
        fi
        
        # Install Docker Compose
        if ! command -v docker-compose &> /dev/null; then
            curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
            chmod +x /usr/local/bin/docker-compose
        fi
        
        # Create deployment directory
        mkdir -p /var/www/psyop.ca
        
        echo "Server environment setup completed"
EOF
    
    success "Server environment configured"
}

# Deploy code to server
deploy_code() {
    log "Deploying code to server..."
    
    # Get current commit hash
    COMMIT_HASH=$(git rev-parse HEAD)
    
    ssh "${SERVER_USER}@${SERVER_HOST}" << EOF
        # Navigate to deployment directory
        cd ${DEPLOY_PATH} || exit 1
        
        # Clone or update repository
        if [ ! -d ".git" ]; then
            git clone \$(git remote get-url origin) .
        else
            git fetch origin
            git reset --hard origin/main
        fi
        
        # Checkout specific commit
        git checkout ${COMMIT_HASH}
        
        echo "Code deployment completed"
EOF
    
    success "Code deployed to server"
}

# Build and start application
build_and_start() {
    log "Building and starting application on server..."
    
    ssh "${SERVER_USER}@${SERVER_HOST}" << EOF
        cd ${DEPLOY_PATH} || exit 1
        
        # Build Docker image
        docker build -t psyop-website:latest .
        
        # Stop existing container if running
        docker stop psyop-website || true
        docker rm psyop-website || true
        
        # Start new container
        docker run -d \
            --name psyop-website \
            --restart unless-stopped \
            -p 8080:8080 \
            psyop-website:latest
        
        # Wait for container to be ready
        sleep 10
        
        # Check if container is running
        if docker ps | grep -q psyop-website; then
            echo "Application started successfully"
        else
            echo "Failed to start application"
            docker logs psyop-website
            exit 1
        fi
EOF
    
    success "Application built and started"
}

# Configure Nginx
setup_nginx() {
    log "Configuring Nginx..."
    
    ssh "${SERVER_USER}@${SERVER_HOST}" << 'EOF'
        # Create Nginx configuration
        cat > /etc/nginx/sites-available/psyop.ca << 'NGINX_CONF'
server {
    listen 80;
    server_name psyop.ca www.psyop.ca;
    
    # Redirect all HTTP traffic to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name psyop.ca www.psyop.ca;
    
    # SSL configuration will be added by certbot
    
    # Proxy to the application
    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeout settings
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;
    }
    
    # Static file serving for better performance
    location /static/ {
        alias /var/www/psyop.ca/static/;
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
    
    # Security headers
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-XSS-Protection "1; mode=block" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header Referrer-Policy "no-referrer-when-downgrade" always;
    add_header Content-Security-Policy "default-src 'self' http: https: data: blob: 'unsafe-inline'" always;
}
NGINX_CONF
        
        # Enable the site
        ln -sf /etc/nginx/sites-available/psyop.ca /etc/nginx/sites-enabled/
        
        # Remove default site if it exists
        rm -f /etc/nginx/sites-enabled/default
        
        # Test nginx configuration
        nginx -t
        
        # Reload nginx
        systemctl reload nginx
        
        echo "Nginx configured"
EOF
    
    success "Nginx configuration completed"
}

# Setup SSL with Let's Encrypt
setup_ssl() {
    log "Setting up SSL certificate..."
    
    ssh "${SERVER_USER}@${SERVER_HOST}" << 'EOF'
        # Obtain SSL certificate
        certbot --nginx -d psyop.ca -d www.psyop.ca --non-interactive --agree-tos --email admin@psyop.ca
        
        # Setup auto-renewal
        echo "0 12 * * * /usr/bin/certbot renew --quiet" | crontab -
        
        echo "SSL certificate configured"
EOF
    
    success "SSL certificate setup completed"
}

# Verify deployment
verify_deployment() {
    log "Verifying deployment..."
    
    # Check if container is running
    ssh "${SERVER_USER}@${SERVER_HOST}" << 'EOF'
        echo "=== Docker Container Status ==="
        docker ps | grep psyop-website
        
        echo "=== Application Health Check ==="
        curl -f http://localhost:8080/ || echo "Health check failed"
        
        echo "=== Nginx Status ==="
        systemctl status nginx --no-pager
        
        echo "=== Disk Usage ==="
        df -h
EOF
    
    # Test external access
    log "Testing external access..."
    if curl -f -s "http://${SERVER_HOST}" > /dev/null; then
        success "Website is accessible via HTTP"
    else
        warn "Website not accessible via HTTP yet"
    fi
    
    success "Deployment verification completed"
}

# Main deployment function
main() {
    log "Starting deployment to server ${SERVER_HOST}"
    
    check_prerequisites
    setup_server
    deploy_code
    build_and_start
    setup_nginx
    setup_ssl
    verify_deployment
    
    success "Deployment completed successfully!"
    
    echo ""
    log "Your website should be available at:"
    log "- https://psyop.ca"
    log "- https://www.psyop.ca"
    echo ""
    log "To check logs: ssh ${SERVER_USER}@${SERVER_HOST} 'docker logs psyop-website'"
    log "To restart: ssh ${SERVER_USER}@${SERVER_HOST} 'docker restart psyop-website'"
}

# Show help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Deploy psyop.ca website to server ${SERVER_HOST}"
    echo ""
    echo "Options:"
    echo "  --setup-only    Only setup server environment"
    echo "  --code-only     Only deploy code (skip server setup)"
    echo "  --verify-only   Only verify deployment"
    echo "  -h, --help      Show this help message"
    echo ""
    echo "Prerequisites:"
    echo "  - SSH access to ${SERVER_HOST}"
    echo "  - Git repository with committed changes"
    echo "  - Domain DNS pointing to ${SERVER_HOST}"
}

# Handle command line arguments
case "${1:-}" in
    --setup-only)
        setup_server
        ;;
    --code-only)
        check_prerequisites
        deploy_code
        build_and_start
        verify_deployment
        ;;
    --verify-only)
        verify_deployment
        ;;
    -h|--help)
        show_help
        exit 0
        ;;
    *)
        main
        ;;
esac
