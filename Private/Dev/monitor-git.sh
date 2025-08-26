#!/usr/bin/env bash
set -euo pipefail

echo "👀 PSYOP Git Monitor - Real-Time Deployment"
echo "==========================================="

# Configuration
GIT_REMOTE=${GIT_REMOTE:-origin}
BRANCH=${BRANCH:-main}
CHECK_INTERVAL=${CHECK_INTERVAL:-30}  # Check every 30 seconds
APP_DIR=${APP_DIR:-/opt/psyop}
SERVICE_NAME=${SERVICE_NAME:-psyop-website}

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

# Function to check if we're in the right directory
check_environment() {
    if [ ! -d ".git" ]; then
        print_error "Not in a git repository. Please run this from the project root."
        exit 1
    fi
    
    if [ ! -d "${APP_DIR}" ]; then
        print_error "App directory ${APP_DIR} not found. Please run this from the correct location."
        exit 1
    fi
    
    print_success "Environment check passed"
}

# Function to get current commit hash
get_current_commit() {
    git rev-parse HEAD
}

# Function to check for remote changes
check_remote_changes() {
    local current_commit=$1
    
    # Fetch latest changes
    git fetch ${GIT_REMOTE} ${BRANCH} >/dev/null 2>&1
    
    # Get remote commit hash
    local remote_commit=$(git rev-parse ${GIT_REMOTE}/${BRANCH})
    
    if [ "$current_commit" != "$remote_commit" ]; then
        echo "$remote_commit"
    else
        echo ""
    fi
}

# Function to deploy updates
deploy_updates() {
    local new_commit=$1
    
    print_status "🔄 New changes detected! Deploying update..."
    print_status "   From: $(git rev-parse --short HEAD)"
    print_status "   To:   $(git rev-parse --short ${new_commit})"
    
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

# Function to show deployment summary
show_deployment_summary() {
    local old_commit=$1
    local new_commit=$2
    
    print_success "🎉 Real-time deployment completed!"
    echo
    echo "📋 Summary:"
    echo "   ✅ Changes pulled from ${GIT_REMOTE}/${BRANCH}"
    echo "   ✅ Project built successfully"
    echo "   ✅ Service restarted and running"
    echo "   ✅ Website updated in real-time"
    echo
    echo "🔄 Commit Update:"
    echo "   From: $(git rev-parse --short ${old_commit})"
    echo "   To:   $(git rev-parse --short ${new_commit})"
    echo
    echo "📊 Monitor the service with:"
    echo "   systemctl status ${SERVICE_NAME}.service"
    echo "   journalctl -u ${SERVICE_NAME}.service -f"
}

# Function to monitor loop
monitor_loop() {
    print_status "Starting monitoring loop..."
    print_status "   Checking every ${CHECK_INTERVAL} seconds"
    print_status "   Monitoring branch: ${GIT_REMOTE}/${BRANCH}"
    print_status "   Press Ctrl+C to stop"
    echo
    
    local current_commit=$(get_current_commit)
    print_status "Current commit: $(git rev-parse --short ${current_commit})"
    echo
    
    while true; do
        # Check for remote changes
        local new_commit=$(check_remote_changes "$current_commit")
        
        if [ -n "$new_commit" ]; then
            local old_commit=$current_commit
            
            # Deploy updates
            if deploy_updates "$new_commit"; then
                show_deployment_summary "$old_commit" "$new_commit"
                current_commit=$new_commit
            else
                print_error "Deployment failed, keeping current version"
            fi
            
            echo
        fi
        
        # Wait before next check
        sleep ${CHECK_INTERVAL}
    done
}

# Function to show usage
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -i, --interval SECONDS  Check interval in seconds (default: 30)"
    echo "  -b, --branch BRANCH     Branch to monitor (default: main)"
    echo "  -r, --remote REMOTE     Git remote to monitor (default: origin)"
    echo "  -h, --help              Show this help message"
    echo
    echo "Environment Variables:"
    echo "  CHECK_INTERVAL          Check interval in seconds"
    echo "  BRANCH                  Branch to monitor"
    echo "  GIT_REMOTE              Git remote to monitor"
    echo "  APP_DIR                 Application directory"
    echo "  SERVICE_NAME            Systemd service name"
    echo
    echo "Examples:"
    echo "  $0                      # Monitor with default settings"
    echo "  $0 -i 60               # Check every 60 seconds"
    echo "  $0 -b develop          # Monitor develop branch"
    echo "  CHECK_INTERVAL=15 $0   # Check every 15 seconds"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -i|--interval)
            CHECK_INTERVAL="$2"
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
    echo "🚀 Starting PSYOP Git Monitor..."
    echo "   Branch: ${BRANCH}"
    echo "   Remote: ${GIT_REMOTE}"
    echo "   Interval: ${CHECK_INTERVAL} seconds"
    echo "   App Directory: ${APP_DIR}"
    echo "   Service: ${SERVICE_NAME}"
    echo
    
    check_environment
    monitor_loop
}

# Trap to clean up on exit
trap 'echo -e "\n${YELLOW}[INFO]${NC} Git monitor stopped"; exit 0' INT TERM

# Run main function
main "$@"
