#!/usr/bin/env bash
set -euo pipefail

echo "🚀 PSYOP Automated Real-Time Deployment"
echo "======================================="

# Configuration
REMOTE_HOST=${REMOTE_HOST:-}
REMOTE_USER=${REMOTE_USER:-root}
APP_USER=${APP_USER:-psyop}
APP_DIR=${APP_DIR:-/opt/psyop}
SERVICE_NAME=${SERVICE_NAME:-psyop-website}
BRANCH=${BRANCH:-main}
GIT_REMOTE=${GIT_REMOTE:-origin}

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
    
    # Check if we're in a git repository
    if [ ! -d ".git" ]; then
        print_error "Not in a git repository. Please run this from the project root."
        exit 1
    fi
    
    # Check if REMOTE_HOST is set
    if [[ -z "${REMOTE_HOST}" ]]; then
        print_error "REMOTE_HOST is required"
        echo "   Usage: REMOTE_HOST=your-server.com ./deploy-auto.sh"
        echo "   Or set in your environment: export REMOTE_HOST=your-server.com"
        exit 1
    fi
    
    # Check SSH connection
    print_status "Testing SSH connection to ${REMOTE_USER}@${REMOTE_HOST}..."
    if ! ssh -o ConnectTimeout=5 -o BatchMode=yes ${REMOTE_USER}@${REMOTE_HOST} exit 2>/dev/null; then
        print_error "Cannot connect to ${REMOTE_USER}@${REMOTE_HOST}"
        print_error "Please check your SSH configuration and try again"
        exit 1
    fi
    print_success "SSH connection successful"
}

# Function to build and test locally
build_and_test() {
    print_status "Building and testing locally..."
    
    # Build the project
    if ! stack build; then
        print_error "Local build failed"
        exit 1
    fi
    
    # Run tests if they exist
    if [ -f "package.yaml" ] && grep -q "tests:" package.yaml; then
        print_status "Running tests..."
        if ! stack test; then
            print_error "Tests failed"
            exit 1
        fi
        print_success "All tests passed"
    fi
    
    print_success "Local build and test completed"
}

# Function to push to main branch
push_to_main() {
    print_status "Pushing changes to ${GIT_REMOTE}/${BRANCH}..."
    
    # Get current branch
    CURRENT_BRANCH=$(git branch --show-current)
    
    if [ "$CURRENT_BRANCH" != "$BRANCH" ]; then
        print_warning "Currently on branch '$CURRENT_BRANCH', switching to '$BRANCH'..."
        git checkout $BRANCH
    fi
    
    # Pull latest changes
    print_status "Pulling latest changes from ${GIT_REMOTE}/${BRANCH}..."
    if ! git pull $GIT_REMOTE $BRANCH; then
        print_error "Failed to pull latest changes"
        exit 1
    fi
    
    # Push changes
    print_status "Pushing changes to ${GIT_REMOTE}/${BRANCH}..."
    if ! git push $GIT_REMOTE $BRANCH; then
        print_error "Failed to push changes"
        exit 1
    fi
    
    print_success "Successfully pushed to ${GIT_REMOTE}/${BRANCH}"
}

# Function to deploy to remote server
deploy_to_server() {
    print_status "Deploying to ${REMOTE_HOST}..."
    
    # Create deployment script on remote
    ssh ${REMOTE_USER}@${REMOTE_HOST} bash -s <<'REMOTE_DEPLOY'
set -euo pipefail

echo "🚀 Starting deployment on remote server..."

# Function to print status
print_status() {
    echo "[INFO] $1"
}

print_success() {
    echo "[SUCCESS] $1"
}

print_error() {
    echo "[ERROR] $1"
}

cd /opt/psyop

# Pull latest changes
print_status "Pulling latest changes from git..."
if ! git pull origin main; then
    print_error "Failed to pull latest changes"
    exit 1
fi

# Build the project
print_status "Building project..."
if ! STACK_ROOT=/opt/psyop/.stack stack build --copy-bins --local-bin-path /opt/psyop/bin; then
    print_error "Build failed"
    exit 1
fi

# Restart the service
print_status "Restarting service..."
if systemctl is-active psyop-website.service >/dev/null 2>&1; then
    systemctl restart psyop-website.service
    print_success "Service restarted"
else
    print_warning "Service not running, starting it..."
    systemctl start psyop-website.service
    print_success "Service started"
fi

# Check service status
print_status "Checking service status..."
if systemctl is-active psyop-website.service >/dev/null 2>&1; then
    print_success "Service is running"
else
    print_error "Service failed to start"
    systemctl status --no-pager psyop-website.service
    exit 1
fi

print_success "Deployment completed successfully"
REMOTE_DEPLOY

    if [ $? -eq 0 ]; then
        print_success "Deployment to ${REMOTE_HOST} completed successfully"
    else
        print_error "Deployment to ${REMOTE_HOST} failed"
        exit 1
    fi
}

# Function to run smoke tests
run_smoke_tests() {
    print_status "Running smoke tests against ${REMOTE_HOST}..."
    
    if [ -f "Private/Dev/smoke-tests.sh" ]; then
        HOST=https://${REMOTE_HOST} bash -c '"$(dirname "$0")"/smoke-tests.sh'
        if [ $? -eq 0 ]; then
            print_success "Smoke tests passed"
        else
            print_error "Smoke tests failed"
            exit 1
        fi
    else
        print_warning "Smoke tests script not found, skipping tests"
    fi
}

# Function to show deployment summary
show_summary() {
    print_success "🎉 Deployment completed successfully!"
    echo
    echo "📋 Summary:"
    echo "   ✅ Local build and tests passed"
    echo "   ✅ Changes pushed to ${GIT_REMOTE}/${BRANCH}"
    echo "   ✅ Deployed to ${REMOTE_HOST}"
    echo "   ✅ Service is running"
    echo
    echo "🌐 Your website is now live at:"
    echo "   https://${REMOTE_HOST}"
    echo
    echo "📊 Monitor the service with:"
    echo "   ssh ${REMOTE_USER}@${REMOTE_HOST} 'systemctl status psyop-website.service'"
    echo "   ssh ${REMOTE_USER}@${REMOTE_HOST} 'journalctl -u psyop-website.service -f'"
}

# Main execution
main() {
    echo "🚀 Starting PSYOP automated deployment..."
    echo "   Target: ${REMOTE_HOST}"
    echo "   Branch: ${BRANCH}"
    echo "   User: ${REMOTE_USER}"
    echo
    
    check_prerequisites
    build_and_test
    push_to_main
    deploy_to_server
    run_smoke_tests
    show_summary
}

# Run main function
main "$@"
