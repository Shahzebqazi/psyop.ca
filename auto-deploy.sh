#!/bin/bash

# PSYOP Website - Auto-Deploy Script
# Automatically deploys website updates and manages the deployment pipeline

set -e

# Configuration
APP_NAME="${APP_NAME:-psyop-website}"
APP_PORT="${APP_PORT:-8080}"
WEBHOOK_PORT="${WEBHOOK_PORT:-9000}"
REPO_URL="${REPO_URL:-https://github.com/Shahzebqazi/psyop.ca.git}"
DEPLOY_USER="${DEPLOY_USER:-psyop}"
APP_DIR="${APP_DIR:-/opt/psyop-website}"
LOG_FILE="${LOG_FILE:-/var/log/psyop-deploy.log}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() {
    echo -e "${BLUE}[INFO]${NC} $1" | tee -a "$LOG_FILE"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "$LOG_FILE"
}

warn() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "$LOG_FILE"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "$LOG_FILE"
    exit 1
}

# Initialize deployment environment
init_deployment() {
    log "Initializing deployment environment..."
    
    # Create app directory if it doesn't exist
    sudo mkdir -p "$APP_DIR"
    sudo chown "$DEPLOY_USER:$DEPLOY_USER" "$APP_DIR"
    
    # Clone repository if it doesn't exist
    if [ ! -d "$APP_DIR/.git" ]; then
        log "Cloning repository..."
        sudo -u "$DEPLOY_USER" git clone "$REPO_URL" "$APP_DIR"
    fi
    
    # Navigate to app directory
    cd "$APP_DIR"
    
    # Set up git configuration
    sudo -u "$DEPLOY_USER" git config --global --add safe.directory "$APP_DIR"
    
    success "Deployment environment initialized"
}

# Deploy the application
deploy_app() {
    log "Starting application deployment..."
    
    cd "$APP_DIR"
    
    # Pull latest changes
    log "Pulling latest changes from main branch..."
    sudo -u "$DEPLOY_USER" git fetch origin
    sudo -u "$DEPLOY_USER" git reset --hard origin/main
    
    # Build the application
    log "Building Haskell application..."
    if ! sudo -u "$DEPLOY_USER" stack build; then
        error "Build failed"
    fi
    
    # Copy built binary to app directory
    log "Installing built application..."
    sudo -u "$DEPLOY_USER" cp .stack-work/dist/*/build/psyop-website-exe/psyop-website-exe ./
    
    # Copy static files
    if [ -d "static" ]; then
        sudo -u "$DEPLOY_USER" cp -r static ./
    fi
    
    # Set proper permissions
    sudo chown -R "$DEPLOY_USER:$DEPLOY_USER" "$APP_DIR"
    sudo chmod +x "$APP_DIR/psyop-website-exe"
    
    # Restart the service
    log "Restarting application service..."
    if sudo systemctl is-active --quiet psyop-website; then
        sudo systemctl restart psyop-website
    else
        sudo systemctl start psyop-website
    fi
    
    success "Application deployed successfully"
}

# Setup webhook server
setup_webhook() {
    log "Setting up webhook server..."
    
    cd "$APP_DIR"
    
    # Copy webhook server files
    if [ -f "webhook-server.py" ]; then
        sudo -u "$DEPLOY_USER" cp webhook-server.py ./
    fi
    
    if [ -f "psyop-webhook.service" ]; then
        sudo -u "$DEPLOY_USER" cp psyop-webhook.service ./
    fi
    
    # Install webhook service
    if [ -f "psyop-webhook.service" ]; then
        sudo cp psyop-webhook.service /etc/systemd/system/
        sudo systemctl daemon-reload
        sudo systemctl enable psyop-webhook
        sudo systemctl restart psyop-webhook
        success "Webhook server setup completed"
    else
        warn "Webhook service file not found, skipping webhook setup"
    fi
}

# Setup nginx configuration
setup_nginx() {
    log "Setting up nginx configuration..."
    
    # Create nginx configuration
    sudo tee /etc/nginx/sites-available/psyop-website > /dev/null << EOF
server {
    listen 80;
    server_name ${DOMAIN_NAME:-psyop.ca} ${DOMAIN_WWW:-www.psyop.ca};
    
    location / {
        proxy_pass http://localhost:$APP_PORT;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
    }
    
    location /static/ {
        alias $APP_DIR/static/;
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
    
    # Webhook endpoint (optional, for monitoring)
    location /webhook-status {
        proxy_pass http://localhost:$WEBHOOK_PORT;
        proxy_set_header Host \$host;
    }
}
EOF

    # Enable site
    sudo ln -sf /etc/nginx/sites-available/psyop-website /etc/nginx/sites-enabled/
    sudo rm -f /etc/nginx/sites-enabled/default
    
    # Test and restart nginx
    sudo nginx -t
    sudo systemctl restart nginx
    
    success "Nginx configuration updated"
}

# Setup systemd service for the main application
setup_app_service() {
    log "Setting up application systemd service..."
    
    sudo tee /etc/systemd/system/psyop-website.service > /dev/null << EOF
[Unit]
Description=PSYOP Website Haskell Application
After=network.target

[Service]
Type=simple
User=$DEPLOY_USER
WorkingDirectory=$APP_DIR
ExecStart=$APP_DIR/psyop-website-exe
Restart=always
RestartSec=3
Environment=PORT=$APP_PORT

[Install]
WantedBy=multi-user.target
EOF

    sudo systemctl daemon-reload
    sudo systemctl enable psyop-website
    
    success "Application service configured"
}

# Configure firewall
setup_firewall() {
    log "Configuring firewall..."
    
    sudo ufw allow ssh
    sudo ufw allow 'Nginx Full'
    sudo ufw allow "$WEBHOOK_PORT/tcp"
    sudo ufw --force enable
    
    success "Firewall configured"
}

# Generate webhook secret
generate_webhook_secret() {
    log "Generating webhook secret..."
    
    WEBHOOK_SECRET=$(openssl rand -hex 32)
    echo "WEBHOOK_SECRET=$WEBHOOK_SECRET" | sudo tee -a /etc/environment
    
    # Update webhook service with new secret
    if [ -f "/etc/systemd/system/psyop-webhook.service" ]; then
        sudo sed -i "s/your-webhook-secret-here/$WEBHOOK_SECRET/" /etc/systemd/system/psyop-webhook.service
        sudo systemctl daemon-reload
        sudo systemctl restart psyop-webhook
    fi
    
    success "Webhook secret generated and configured"
    log "Webhook secret: $WEBHOOK_SECRET"
    log "Add this secret to your GitHub repository webhook settings"
}

# Show deployment status
show_status() {
    log "Checking deployment status..."
    
    echo ""
    echo "=== Deployment Status ==="
    
    # Check application service
    if sudo systemctl is-active --quiet psyop-website; then
        echo "✅ Application service: RUNNING"
    else
        echo "❌ Application service: NOT RUNNING"
    fi
    
    # Check webhook service
    if sudo systemctl is-active --quiet psyop-webhook; then
        echo "✅ Webhook service: RUNNING"
    else
        echo "❌ Webhook service: NOT RUNNING"
    fi
    
    # Check nginx
    if sudo systemctl is-active --quiet nginx; then
        echo "✅ Nginx: RUNNING"
    else
        echo "❌ Nginx: NOT RUNNING"
    fi
    
    # Check ports
    if netstat -tlnp | grep -q ":$APP_PORT "; then
        echo "✅ Application port $APP_PORT: LISTENING"
    else
        echo "❌ Application port $APP_PORT: NOT LISTENING"
    fi
    
    if netstat -tlnp | grep -q ":$WEBHOOK_PORT "; then
        echo "✅ Webhook port $WEBHOOK_PORT: LISTENING"
    else
        echo "❌ Webhook port $WEBHOOK_PORT: NOT LISTENING"
    fi
    
    echo ""
    echo "=== Next Steps ==="
    echo "1. Configure GitHub webhook:"
    echo "   - URL: http://$(curl -s ifconfig.me):$WEBHOOK_PORT"
    echo "   - Secret: Check /etc/environment for WEBHOOK_SECRET"
    echo "2. Test deployment: git push to main branch"
    echo "3. Monitor logs: sudo journalctl -u psyop-website -f"
    echo "4. Check webhook logs: sudo journalctl -u psyop-webhook -f"
}

# Main deployment function
main() {
    log "Starting PSYOP website auto-deployment setup..."
    
    # Check if running as root
    if [ "$EUID" -eq 0 ]; then
        error "Please don't run this script as root. Use a regular user with sudo privileges."
    fi
    
    init_deployment
    setup_app_service
    setup_nginx
    setup_webhook
    setup_firewall
    generate_webhook_secret
    deploy_app
    show_status
    
    success "Auto-deployment setup completed successfully!"
}

# Show help
show_help() {
    echo "Usage: $0 [COMMAND]"
    echo ""
    echo "PSYOP Website Auto-Deployment Script"
    echo ""
    echo "Commands:"
    echo "  deploy      Deploy the application (default)"
    echo "  status      Show deployment status"
    echo "  setup       Setup deployment environment only"
    echo "  help        Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0           # Full deployment setup"
    echo "  $0 deploy    # Deploy application"
    echo "  $0 status    # Check status"
    echo ""
    echo "Prerequisites:"
    echo "  - Ubuntu/Debian server"
    echo "  - User with sudo privileges"
    echo "  - SSH access to server"
}

# Handle command line arguments
case "${1:-deploy}" in
    deploy)
        main
        ;;
    status)
        show_status
        ;;
    setup)
        init_deployment
        setup_app_service
        setup_nginx
        setup_webhook
        setup_firewall
        generate_webhook_secret
        success "Setup completed. Run '$0 deploy' to deploy the application."
        ;;
    -h|--help|help)
        show_help
        exit 0
        ;;
    *)
        echo "Unknown command: $1"
        show_help
        exit 1
        ;;
esac
