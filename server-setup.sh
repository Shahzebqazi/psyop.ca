#!/bin/bash

# PSYOP Website - DigitalOcean Server Setup Script
# Run this script on your DigitalOcean droplet to prepare it for hosting

set -e

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

# Update system packages
update_system() {
    log "Updating system packages..."
    sudo apt-get update
    sudo apt-get upgrade -y
    success "System updated successfully"
}

# Install required packages
install_packages() {
    log "Installing required packages..."
    sudo apt-get install -y \
        curl \
        wget \
        git \
        build-essential \
        libffi-dev \
        libgmp-dev \
        libtinfo-dev \
        zlib1g-dev \
        nginx \
        ufw \
        certbot \
        python3-certbot-nginx
    success "Packages installed successfully"
}

# Configure firewall
configure_firewall() {
    log "Configuring firewall..."
    sudo ufw default deny incoming
    sudo ufw default allow outgoing
    sudo ufw allow ssh
    sudo ufw allow 'Nginx Full'
    sudo ufw --force enable
    success "Firewall configured successfully"
}

# Create application user
create_app_user() {
    log "Creating application user..."
    sudo useradd -r -s /bin/false psyop || true
    sudo mkdir -p /opt/psyop-website
    sudo chown psyop:psyop /opt/psyop-website
    success "Application user created successfully"
}

# Install Haskell Stack
install_haskell_stack() {
    log "Installing Haskell Stack..."
    curl -sSL https://get.haskellstack.org/ | sh
    success "Haskell Stack installed successfully"
}

# Configure nginx
configure_nginx() {
    log "Configuring nginx..."
    
    # Create nginx configuration
    sudo tee /etc/nginx/sites-available/psyop-website > /dev/null << 'EOF'
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

    # Enable site and disable default
    sudo ln -sf /etc/nginx/sites-available/psyop-website /etc/nginx/sites-enabled/
    sudo rm -f /etc/nginx/sites-enabled/default
    
    # Test configuration
    sudo nginx -t
    
    # Restart nginx
    sudo systemctl restart nginx
    sudo systemctl enable nginx
    
    success "Nginx configured successfully"
}

# Create systemd service
create_systemd_service() {
    log "Creating systemd service..."
    
    sudo tee /etc/systemd/system/psyop-website.service > /dev/null << 'EOF'
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

    sudo systemctl daemon-reload
    sudo systemctl enable psyop-website
    
    success "Systemd service created successfully"
}

# Setup SSL with Let's Encrypt
setup_ssl() {
    log "Setting up SSL with Let's Encrypt..."
    
    if [ -n "$DOMAIN_NAME" ]; then
        log "Domain name provided: $DOMAIN_NAME"
        sudo certbot --nginx -d "$DOMAIN_NAME" -d "www.$DOMAIN_NAME" --non-interactive --agree-tos --email admin@psyop.ca
        success "SSL certificate obtained successfully"
    else
        warn "No domain name provided. SSL setup skipped."
        warn "To setup SSL later, run: sudo certbot --nginx -d yourdomain.com"
    fi
}

# Show final instructions
show_final_instructions() {
    success "Server setup completed successfully!"
    echo ""
    echo "Next steps:"
    echo "1. Deploy your application using the deploy-digitalocean.sh script"
    echo "2. Configure your domain DNS to point to this server"
    echo "3. Run SSL setup: sudo certbot --nginx -d yourdomain.com"
    echo ""
    echo "Useful commands:"
    echo "- Check service status: sudo systemctl status psyop-website"
    echo "- View logs: sudo journalctl -u psyop-website -f"
    echo "- Check nginx status: sudo systemctl status nginx"
    echo "- View nginx logs: sudo tail -f /var/log/nginx/access.log"
}

# Main setup function
main() {
    log "Starting PSYOP website server setup..."
    
    # Check if running as root
    if [ "$EUID" -eq 0 ]; then
        error "Please don't run this script as root. Use a regular user with sudo privileges."
    fi
    
    update_system
    install_packages
    configure_firewall
    create_app_user
    install_haskell_stack
    configure_nginx
    create_systemd_service
    
    # Setup SSL if domain is provided
    if [ -n "$DOMAIN_NAME" ]; then
        setup_ssl
    fi
    
    show_final_instructions
}

# Show help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Setup DigitalOcean server for PSYOP website hosting"
    echo ""
    echo "Options:"
    echo "  -d, --domain DOMAIN    Domain name for SSL setup"
    echo "  -h, --help             Show this help message"
    echo ""
    echo "Environment variables:"
    echo "  DOMAIN_NAME            Domain name for SSL setup"
    echo ""
    echo "Examples:"
    echo "  $0"
    echo "  DOMAIN_NAME=psyop.ca $0"
    echo "  $0 -d psyop.ca"
    echo ""
    echo "Prerequisites:"
    echo "  - Ubuntu/Debian server"
    echo "  - User with sudo privileges"
    echo "  - SSH access to server"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--domain)
            DOMAIN_NAME="$2"
            shift 2
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

# Run main setup
main
