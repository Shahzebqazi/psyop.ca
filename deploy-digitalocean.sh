#!/bin/bash

# PSYOP Website - DigitalOcean Server Deployment Script
# This script deploys the Haskell website to a DigitalOcean droplet

set -e

# Configuration
SERVER_IP="${SERVER_IP:-}"
SERVER_USER="${SERVER_USER:-root}"
APP_NAME="psyop-website"
APP_PORT="8080"
NGINX_PORT="80"

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
    
    if [ -z "$SERVER_IP" ]; then
        error "SERVER_IP environment variable is not set. Please set it to your DigitalOcean droplet IP."
    fi
    
    if ! command -v ssh &> /dev/null; then
        error "SSH is not available. Please install OpenSSH client."
    fi
    
    if ! command -v scp &> /dev/null; then
        error "SCP is not available. Please install OpenSSH client."
    fi
    
    success "Prerequisites check completed"
}

# Build the Haskell application
build_application() {
    log "Building Haskell application..."
    
    if ! stack build; then
        error "Failed to build the application"
    fi
    
    success "Application built successfully"
}

# Create deployment package
create_deployment_package() {
    log "Creating deployment package..."
    
    # Create deployment directory
    rm -rf deploy-package
    mkdir -p deploy-package
    
    # Copy built binary
    cp .stack-work/dist/*/build/psyop-website-exe/psyop-website-exe deploy-package/
    
    # Copy static files
    cp -r static deploy-package/
    
    # Copy systemd service file
    cat > deploy-package/psyop-website.service << 'EOF'
[Unit]
Description=PSYOP Website Haskell Application
After=network.target

[Service]
Type=simple
User=psyop
WorkingDirectory=/opt/psyop-website
ExecStart=/opt/psyop-website/psyop-website-exe
Restart=always
RestartSec=3
Environment=PORT=8080

[Install]
WantedBy=multi-user.target
EOF

    # Copy nginx configuration
    cat > deploy-package/nginx-psyop << 'EOF'
server {
    listen 80;
    server_name psyop.ca www.psyop.ca;
    
    location / {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
    
    location /static/ {
        alias /opt/psyop-website/static/;
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
}
EOF

    success "Deployment package created"
}

# Deploy to server
deploy_to_server() {
    log "Deploying to DigitalOcean server: $SERVER_IP"
    
    # Create app directory on server
    ssh -o StrictHostKeyChecking=no "$SERVER_USER@$SERVER_IP" << 'EOF'
        sudo mkdir -p /opt/psyop-website
        sudo useradd -r -s /bin/false psyop || true
        sudo chown psyop:psyop /opt/psyop-website
EOF
    
    # Copy files to server
    log "Copying files to server..."
    scp -r deploy-package/* "$SERVER_USER@$SERVER_IP:/tmp/psyop-website/"
    
    # Install and configure on server
    ssh "$SERVER_USER@$SERVER_IP" << 'EOF'
        # Move files to final location
        sudo mv /tmp/psyop-website/* /opt/psyop-website/
        sudo chown -R psyop:psyop /opt/psyop-website
        sudo chmod +x /opt/psyop-website/psyop-website-exe
        
        # Install systemd service
        sudo mv /opt/psyop-website/psyop-website.service /etc/systemd/system/
        sudo systemctl daemon-reload
        sudo systemctl enable psyop-website
        sudo systemctl restart psyop-website
        
        # Install nginx if not present
        if ! command -v nginx &> /dev/null; then
            sudo apt-get update
            sudo apt-get install -y nginx
        fi
        
        # Configure nginx
        sudo mv /opt/psyop-website/nginx-psyop /etc/nginx/sites-available/psyop-website
        sudo ln -sf /etc/nginx/sites-available/psyop-website /etc/nginx/sites-enabled/
        sudo rm -f /etc/nginx/sites-enabled/default
        sudo nginx -t
        sudo systemctl restart nginx
        
        # Configure firewall
        sudo ufw allow 80/tcp
        sudo ufw allow 22/tcp
        sudo ufw --force enable
        
        # Clean up
        rm -rf /tmp/psyop-website
EOF
    
    success "Deployment completed successfully"
}

# Verify deployment
verify_deployment() {
    log "Verifying deployment..."
    
    # Check if service is running
    if ssh "$SERVER_USER@$SERVER_IP" "sudo systemctl is-active --quiet psyop-website"; then
        success "PSYOP website service is running"
    else
        error "PSYOP website service is not running"
    fi
    
    # Check if nginx is running
    if ssh "$SERVER_USER@$SERVER_IP" "sudo systemctl is-active --quiet nginx"; then
        success "Nginx is running"
    else
        error "Nginx is not running"
    fi
    
    # Test website
    log "Testing website..."
    if curl -f "http://$SERVER_IP" > /dev/null 2>&1; then
        success "Website is accessible at http://$SERVER_IP"
    else
        warn "Website might not be accessible yet. Please wait a few moments and try again."
    fi
}

# Show help
show_help() {
    echo "Usage: $0"
    echo ""
    echo "Deploy PSYOP website to DigitalOcean server"
    echo ""
    echo "Environment variables:"
    echo "  SERVER_IP     DigitalOcean droplet IP address (required)"
    echo "  SERVER_USER   SSH user (default: root)"
    echo ""
    echo "Examples:"
    echo "  SERVER_IP=123.456.789.012 $0"
    echo "  SERVER_IP=123.456.789.012 SERVER_USER=ubuntu $0"
    echo ""
    echo "Prerequisites:"
    echo "  - SSH access to DigitalOcean droplet"
    echo "  - Haskell Stack installed locally"
    echo "  - Server IP address configured"
}

# Main deployment function
main() {
    log "Starting deployment to DigitalOcean server"
    log "Server IP: $SERVER_IP"
    log "Server User: $SERVER_USER"
    
    check_prerequisites
    build_application
    create_deployment_package
    deploy_to_server
    verify_deployment
    
    success "Deployment completed successfully!"
    
    log "Your website should be available at:"
    log "- http://$SERVER_IP"
    log "- https://psyop.ca (after DNS configuration)"
    
    log "To check service status:"
    log "ssh $SERVER_USER@$SERVER_IP 'sudo systemctl status psyop-website'"
    log "ssh $SERVER_USER@$SERVER_IP 'sudo systemctl status nginx'"
}

# Handle command line arguments
case "${1:-}" in
    -h|--help)
        show_help
        exit 0
        ;;
    *)
        main
        ;;
esac
