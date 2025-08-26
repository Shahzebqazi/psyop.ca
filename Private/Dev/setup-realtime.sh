#!/usr/bin/env bash
set -euo pipefail

echo "🚀 PSYOP Real-Time Deployment Setup"
echo "==================================="

# Configuration
REMOTE_HOST=${REMOTE_HOST:-}
REMOTE_USER=${REMOTE_USER:-root}
WEBHOOK_SECRET=${WEBHOOK_SECRET:-}
WEBHOOK_PORT=${WEBHOOK_PORT:-9000}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check prerequisites
check_prerequisites() {
    print_status "Checking prerequisites..."
    
    # Check if we're on the server
    if [ ! -d "/opt/psyop" ]; then
        print_error "This script must be run on the production server"
        print_error "Please run it from /opt/psyop/psyop.ca on your server"
        exit 1
    fi
    
    # Check if we're in the right directory
    if [ ! -d ".git" ]; then
        print_error "Not in a git repository. Please run this from /opt/psyop/psyop.ca"
        exit 1
    fi
    
    print_success "Prerequisites check passed"
}

# Function to generate webhook secret
generate_webhook_secret() {
    if [ -z "$WEBHOOK_SECRET" ]; then
        print_status "Generating webhook secret..."
        WEBHOOK_SECRET=$(openssl rand -hex 32)
        print_success "Webhook secret generated: $WEBHOOK_SECRET"
    else
        print_status "Using provided webhook secret"
    fi
}

# Function to install systemd services
install_systemd_services() {
    print_status "Installing systemd services..."
    
    # Copy service files
    sudo cp Private/Dev/psyop-git-monitor.service /etc/systemd/system/
    sudo cp Private/Dev/psyop-webhook.service /etc/systemd/system/
    
    # Update webhook service with secret
    sudo sed -i "s/Environment=WEBHOOK_SECRET=/Environment=WEBHOOK_SECRET=$WEBHOOK_SECRET/" /etc/systemd/system/psyop-webhook.service
    
    # Reload systemd
    sudo systemctl daemon-reload
    
    print_success "Systemd services installed"
}

# Function to enable and start services
start_services() {
    print_status "Starting real-time deployment services..."
    
    # Enable services
    sudo systemctl enable psyop-git-monitor.service
    sudo systemctl enable psyop-webhook.service
    
    # Start services
    sudo systemctl start psyop-git-monitor.service
    sudo systemctl start psyop-webhook.service
    
    # Check status
    print_status "Checking service status..."
    
    if systemctl is-active psyop-git-monitor.service >/dev/null 2>&1; then
        print_success "Git monitor service is running"
    else
        print_error "Git monitor service failed to start"
        systemctl status --no-pager psyop-git-monitor.service
    fi
    
    if systemctl is-active psyop-webhook.service >/dev/null 2>&1; then
        print_success "Webhook service is running"
    else
        print_error "Webhook service failed to start"
        systemctl status --no-pager psyop-webhook.service
    fi
}

# Function to configure firewall
configure_firewall() {
    print_status "Configuring firewall for webhook..."
    
    # Check if ufw is available
    if command -v ufw >/dev/null 2>&1; then
        sudo ufw allow $WEBHOOK_PORT/tcp
        print_success "Firewall rule added for port $WEBHOOK_PORT"
    else
        print_warning "ufw not available, please manually configure firewall for port $WEBHOOK_PORT"
    fi
}

# Function to create GitHub webhook configuration
create_github_webhook_config() {
    print_status "Creating GitHub webhook configuration..."
    
    local server_ip=$(curl -s ifconfig.me 2>/dev/null || echo "YOUR_SERVER_IP")
    
    cat > "Private/Dev/github-webhook-setup.md" << EOF
# GitHub Webhook Configuration

To enable real-time deployment, configure a webhook in your GitHub repository:

## Webhook Settings

1. Go to your GitHub repository: https://github.com/Shahzebqazi/psyop.ca
2. Click **Settings** → **Webhooks** → **Add webhook**
3. Configure the webhook:

   - **Payload URL**: http://${server_ip}:${WEBHOOK_PORT}/webhook
   - **Content type**: application/json
   - **Secret**: ${WEBHOOK_SECRET}
   - **Events**: Just the push event
   - **Branch**: main

## Security Notes

- The webhook secret is: \`${WEBHOOK_SECRET}\`
- Keep this secret secure and don't share it publicly
- The webhook server validates all incoming requests
- Only pushes to the main branch will trigger deployment

## Testing

After setting up the webhook, make a small change to your repository and push to main. The website should update automatically within seconds.

## Monitoring

Check the status of real-time deployment services:

\`\`\`bash
# Check git monitor service
systemctl status psyop-git-monitor.service

# Check webhook service  
systemctl status psyop-webhook.service

# View logs
journalctl -u psyop-git-monitor.service -f
journalctl -u psyop-webhook.service -f
\`\`\`
EOF

    print_success "GitHub webhook configuration created: Private/Dev/github-webhook-setup.md"
}

# Function to show final instructions
show_final_instructions() {
    print_success "🎉 Real-time deployment setup completed!"
    echo
    echo "📋 What's been set up:"
    echo "   ✅ Git monitor service (checks every 30 seconds)"
    echo "   ✅ Webhook server (instant deployment on push)"
    echo "   ✅ Systemd services (auto-start on boot)"
    echo "   ✅ Firewall configuration"
    echo
    echo "🔧 Next steps:"
    echo "   1. Configure GitHub webhook using: Private/Dev/github-webhook-setup.md"
    echo "   2. Test by pushing a change to main branch"
    echo "   3. Monitor services for any issues"
    echo
    echo "📊 Service status:"
    echo "   Git Monitor: $(systemctl is-active psyop-git-monitor.service)"
    echo "   Webhook:     $(systemctl is-active psyop-webhook.service)"
    echo
    echo "📝 Important information:"
    echo "   Webhook URL: http://$(curl -s ifconfig.me 2>/dev/null || echo "YOUR_SERVER_IP"):${WEBHOOK_PORT}/webhook"
    echo "   Webhook Secret: ${WEBHOOK_SECRET}"
    echo "   Branch monitored: main"
    echo
    echo "🚀 Your website will now update automatically when you push to main!"
}

# Main execution
main() {
    echo "🚀 Starting PSYOP real-time deployment setup..."
    echo "   Webhook Port: $WEBHOOK_PORT"
    echo "   Webhook Secret: ${WEBHOOK_SECRET:-"Will be generated"}"
    echo
    
    check_prerequisites
    generate_webhook_secret
    install_systemd_services
    start_services
    configure_firewall
    create_github_webhook_config
    show_final_instructions
}

# Run main function
main "$@"
