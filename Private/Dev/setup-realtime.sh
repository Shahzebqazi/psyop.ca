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

# Function to install systemd service
install_systemd_service() {
    print_status "Installing systemd service..."
    
    # Copy service file
    sudo cp Private/Dev/psyop-git-monitor.service /etc/systemd/system/
    
    # Reload systemd
    sudo systemctl daemon-reload
    
    print_success "Systemd service installed"
}

# Function to enable and start service
start_service() {
    print_status "Starting real-time deployment service..."
    
    # Enable service
    sudo systemctl enable psyop-git-monitor.service
    
    # Start service
    sudo systemctl start psyop-git-monitor.service
    
    # Check status
    print_status "Checking service status..."
    
    if systemctl is-active psyop-git-monitor.service >/dev/null 2>&1; then
        print_success "Git monitor service is running"
    else
        print_error "Git monitor service failed to start"
        systemctl status --no-pager psyop-git-monitor.service
        exit 1
    fi
}

# Function to show final instructions
show_final_instructions() {
    print_success "🎉 Real-time deployment setup completed!"
    echo
    echo "📋 What's been set up:"
    echo "   ✅ Git monitor service (checks every 30 seconds)"
    echo "   ✅ Systemd service (auto-start on boot)"
    echo "   ✅ Automatic deployment when changes are pushed to main"
    echo
    echo "🔧 How it works:"
    echo "   1. Service monitors origin/main branch every 30 seconds"
    echo "   2. When changes are detected, automatically:"
    echo "      - Pulls latest code"
    echo "      - Builds the project"
    echo "      - Restarts the website service"
    echo "      - Runs smoke tests"
    echo
    echo "📊 Service status:"
    echo "   Git Monitor: $(systemctl is-active psyop-git-monitor.service)"
    echo
    echo "📝 Monitor the service with:"
    echo "   systemctl status psyop-git-monitor.service"
    echo "   journalctl -u psyop-git-monitor.service -f"
    echo
    echo "🚀 Your website will now update automatically when you push to main!"
    echo "   Test it by making a change and pushing to the main branch!"
}

# Main execution
main() {
    echo "🚀 Starting PSYOP real-time deployment setup..."
    echo "   This will set up automatic deployment when you push to main"
    echo
    
    check_prerequisites
    install_systemd_service
    start_service
    show_final_instructions
}

# Run main function
main "$@"
