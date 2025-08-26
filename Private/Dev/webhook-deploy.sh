#!/usr/bin/env bash
set -euo pipefail

echo "🔗 PSYOP Webhook Deployment - Instant Updates"
echo "============================================="

# Configuration
WEBHOOK_PORT=${WEBHOOK_PORT:-9000}
WEBHOOK_SECRET=${WEBHOOK_SECRET:-}
APP_DIR=${APP_DIR:-/opt/psyop}
SERVICE_NAME=${SERVICE_NAME:-psyop-website}
GIT_REMOTE=${GIT_REMOTE:-origin}
BRANCH=${BRANCH:-main}

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

# Function to validate webhook payload
validate_webhook() {
    local payload="$1"
    local signature="$2"
    
    if [ -z "$WEBHOOK_SECRET" ]; then
        print_warning "No webhook secret configured, skipping validation"
        return 0
    fi
    
    # Calculate expected signature (GitHub-style)
    local expected_signature=$(echo -n "$payload" | openssl dgst -sha256 -hmac "$WEBHOOK_SECRET" | cut -d' ' -f2)
    
    if [ "$signature" = "sha256=$expected_signature" ]; then
        print_success "Webhook signature validated"
        return 0
    else
        print_error "Invalid webhook signature"
        return 1
    fi
}

# Function to check if webhook is for main branch
is_main_branch_webhook() {
    local payload="$1"
    
    # Extract branch from webhook payload (GitHub format)
    local branch=$(echo "$payload" | jq -r '.ref // empty' 2>/dev/null | sed 's|refs/heads/||')
    
    if [ "$branch" = "$BRANCH" ]; then
        print_status "Webhook is for $BRANCH branch"
        return 0
    else
        print_warning "Webhook is for $branch branch, ignoring"
        return 1
    fi
}

# Function to deploy updates
deploy_updates() {
    print_status "🚀 Webhook triggered! Deploying updates..."
    
    # Change to app directory
    cd "${APP_DIR}/psyop.ca"
    
    # Pull latest changes
    print_status "Pulling latest changes..."
    if ! git pull ${GIT_REMOTE} ${BRANCH}; then
        print_error "Failed to pull latest changes"
        return 1
    fi
    
    # Build the project
    print_status "Building project..."
    if ! STACK_ROOT=${APP_DIR}/.stack stack build --copy-bins --local-bin-path ${APP_DIR}/bin; then
        print_error "Build failed"
        return 1
    fi
    
    # Restart the service
    print_status "Restarting service..."
    if systemctl is-active ${SERVICE_NAME}.service >/dev/null 2>&1; then
        systemctl restart ${SERVICE_NAME}.service
        print_success "Service restarted"
    else
        print_warning "Service not running, starting it..."
        systemctl start ${SERVICE_NAME}.service
        print_success "Service started"
    fi
    
    # Check service status
    print_status "Checking service status..."
    if systemctl is-active ${SERVICE_NAME}.service >/dev/null 2>&1; then
        print_success "Service is running"
        
        # Run smoke tests
        if [ -f "Private/Dev/smoke-tests.sh" ]; then
            print_status "Running smoke tests..."
            if HOST=https://localhost bash Private/Dev/smoke-tests.sh; then
                print_success "Smoke tests passed"
            else
                print_warning "Smoke tests failed, but service is running"
            fi
        fi
        
        return 0
    else
        print_error "Service failed to start"
        systemctl status --no-pager ${SERVICE_NAME}.service
        return 1
    fi
}

# Function to handle webhook request
handle_webhook() {
    local method="$1"
    local path="$2"
    
    if [ "$method" = "POST" ] && [ "$path" = "/webhook" ]; then
        print_status "Webhook request received"
        
        # Read request body
        local payload=""
        while IFS= read -r line; do
            payload="$payload$line"
        done
        
        # Read headers
        local signature=""
        while IFS= read -r line; do
            if [[ "$line" =~ ^X-Hub-Signature-256:\ (.*)$ ]]; then
                signature="${BASH_REMATCH[1]}"
            fi
        done
        
        # Validate webhook
        if ! validate_webhook "$payload" "$signature"; then
            echo "HTTP/1.1 401 Unauthorized"
            echo "Content-Type: text/plain"
            echo
            echo "Invalid webhook signature"
            return
        fi
        
        # Check if it's for main branch
        if ! is_main_branch_webhook "$payload"; then
            echo "HTTP/1.1 200 OK"
            echo "Content-Type: text/plain"
            echo
            echo "Webhook received but not for $BRANCH branch"
            return
        fi
        
        # Deploy updates
        if deploy_updates; then
            echo "HTTP/1.1 200 OK"
            echo "Content-Type: text/plain"
            echo
            echo "Deployment successful"
            print_success "Webhook deployment completed"
        else
            echo "HTTP/1.1 500 Internal Server Error"
            echo "Content-Type: text/plain"
            echo
            echo "Deployment failed"
            print_error "Webhook deployment failed"
        fi
    else
        echo "HTTP/1.1 404 Not Found"
        echo "Content-Type: text/plain"
        echo
        echo "Not found"
    fi
}

# Function to start webhook server
start_webhook_server() {
    print_status "Starting webhook server on port $WEBHOOK_PORT..."
    print_status "   Webhook URL: http://localhost:$WEBHOOK_PORT/webhook"
    print_status "   Branch: $BRANCH"
    print_status "   Press Ctrl+C to stop"
    echo
    
    # Create log directory
    mkdir -p /var/log/psyop
    
    # Start webhook server
    while true; do
        # Listen for incoming connections
        local request=$(nc -l -p $WEBHOOK_PORT 2>/dev/null || true)
        
        if [ -n "$request" ]; then
            # Parse HTTP request
            local method=$(echo "$request" | head -1 | cut -d' ' -f1)
            local path=$(echo "$request" | head -1 | cut -d' ' -f2)
            
            # Handle webhook
            handle_webhook "$method" "$path"
        fi
    done
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -p, --port PORT         Webhook port (default: 9000)"
    echo "  -s, --secret SECRET     Webhook secret for validation"
    echo "  -b, --branch BRANCH     Branch to monitor (default: main)"
    echo "  -r, --remote REMOTE     Git remote to monitor (default: origin)"
    echo "  -h, --help              Show this help message"
    echo
    echo "Environment Variables:"
    echo "  WEBHOOK_PORT            Webhook port"
    echo "  WEBHOOK_SECRET          Webhook secret for validation"
    echo "  BRANCH                  Branch to monitor"
    echo "  GIT_REMOTE              Git remote to monitor"
    echo "  APP_DIR                 Application directory"
    echo "  SERVICE_NAME            Systemd service name"
    echo
    echo "Examples:"
    echo "  $0                      # Start with default settings"
    echo "  $0 -p 8080             # Use port 8080"
    echo "  $0 -s mysecret         # Set webhook secret"
    echo "  WEBHOOK_SECRET=secret $0 # Set secret via environment"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -p|--port)
            WEBHOOK_PORT="$2"
            shift 2
            ;;
        -s|--secret)
            WEBHOOK_SECRET="$2"
            shift 2
            ;;
        -b|--branch)
            BRANCH="$2"
            shift 2
            ;;
        -r|--remote)
            GIT_REMOTE="$2"
            shift 2
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            print_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Main execution
main() {
    echo "🚀 Starting PSYOP Webhook Server..."
    echo "   Port: $WEBHOOK_PORT"
    echo "   Branch: $BRANCH"
    echo "   Remote: $GIT_REMOTE"
    echo "   App Directory: $APP_DIR"
    echo "   Service: $SERVICE_NAME"
    echo
    
    if [ -n "$WEBHOOK_SECRET" ]; then
        print_status "Webhook secret configured for validation"
    else
        print_warning "No webhook secret configured - no validation"
    fi
    
    start_webhook_server
}

# Trap to clean up on exit
trap 'echo -e "\n${YELLOW}[INFO]${NC} Webhook server stopped"; exit 0' INT TERM

# Run main function
main "$@"
