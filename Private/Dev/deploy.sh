#!/usr/bin/env bash
set -euo pipefail

echo "🚀 PSYOP Deployment Helper"
echo "=========================="

# Configuration
REMOTE_HOST=${REMOTE_HOST:-}
REMOTE_USER=${REMOTE_USER:-root}
APP_USER=${APP_USER:-psyop}
APP_DIR=${APP_DIR:-/opt/psyop}
SERVICE_NAME=${SERVICE_NAME:-psyop-website}
BRANCH=${BRANCH:-main}
DEPLOYMENT_MODE=${DEPLOYMENT_MODE:-manual}

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

# Function to show usage
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -m, --mode MODE         Deployment mode: manual, auto, realtime (default: manual)"
    echo "  -h, --host HOST         Remote host IP/domain"
    echo "  -u, --user USER         SSH username (default: root)"
    echo "  -b, --branch BRANCH     Branch to deploy (default: main)"
    echo "  --help                  Show this help message"
    echo
    echo "Deployment Modes:"
    echo "  manual     - Traditional deployment (default)"
    echo "  auto       - Automated deployment with git push"
    echo "  realtime   - Set up real-time deployment services"
    echo
    echo "Environment Variables:"
    echo "  REMOTE_HOST             Remote server IP/domain"
    echo "  REMOTE_USER             SSH username"
    echo "  DEPLOYMENT_MODE         Deployment mode"
    echo "  BRANCH                  Branch to deploy"
    echo
    echo "Examples:"
    echo "  $0                      # Manual deployment"
    echo "  $0 -m auto             # Automated deployment"
    echo "  $0 -m realtime         # Set up real-time deployment"
    echo "  $0 -h 192.168.1.100    # Deploy to specific host"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -m|--mode)
            DEPLOYMENT_MODE="$2"
            shift 2
            ;;
        -h|--host)
            REMOTE_HOST="$2"
            shift 2
            ;;
        -u|--user)
            REMOTE_USER="$2"
            shift 2
            ;;
        -b|--branch)
            BRANCH="$2"
            shift 2
            ;;
        --help)
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

# Function to check prerequisites
check_prerequisites() {
    print_status "Checking prerequisites..."
    
    if [[ "$DEPLOYMENT_MODE" == "realtime" ]]; then
        # For realtime mode, we need to be on the server
        if [ ! -d "/opt/psyop" ]; then
            print_error "Realtime mode must be run on the production server"
            print_error "Please run it from /opt/psyop/psyop.ca on your server"
            exit 1
        fi
        return 0
    fi
    
    # For other modes, we need REMOTE_HOST
    if [[ -z "${REMOTE_HOST}" ]]; then
        print_error "REMOTE_HOST is required for manual and auto modes"
        echo "   Usage: REMOTE_HOST=your-server.com $0"
        echo "   Or use: $0 -h your-server.com"
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

# Function to run realtime setup
setup_realtime_deployment() {
    print_status "Setting up real-time deployment..."
    
    if [ -f "Private/Dev/setup-realtime.sh" ]; then
        bash Private/Dev/setup-realtime.sh
    else
        print_error "Real-time setup script not found"
        exit 1
    fi
}

# Function to run automated deployment
run_automated_deployment() {
    print_status "Running automated deployment..."
    
    if [ -f "Private/Dev/deploy-auto.sh" ]; then
        REMOTE_HOST="$REMOTE_HOST" bash Private/Dev/deploy-auto.sh
    else
        print_error "Automated deployment script not found"
        exit 1
    fi
}

# Function to run manual deployment
run_manual_deployment() {
    print_status "Running manual deployment..."
    
    # Original deployment logic
    ssh -o StrictHostKeyChecking=no ${REMOTE_USER}@${REMOTE_HOST} bash -s <<'REMOTE_BOOTSTRAP'
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive

echo "[1/6] Removing nginx if present"
if systemctl is-enabled nginx >/dev/null 2>&1 || systemctl is-active nginx >/dev/null 2>&1; then
  systemctl stop nginx || true
  systemctl disable nginx || true
fi
apt-get update -y
apt-get purge -y nginx nginx-common nginx-core || true
apt-get autoremove -y || true

echo "[2/6] Installing prerequisites"
apt-get install -y curl git ca-certificates build-essential libgmp-dev libtinfo-dev zlib1g-dev xz-utils jq
# Image optimization tools (best-effort)
apt-get install -y jpegoptim optipng svgcleaner || true

echo "[3/6] Installing Stack if missing"
if ! command -v stack >/dev/null 2>&1; then
  curl -sSL https://get.haskellstack.org/ | sh
fi

echo "[4/6] Creating app user and directories"
id -u psyop >/dev/null 2>&1 || useradd -r -s /usr/sbin/nologin -d /opt/psyop psyop
mkdir -p /opt/psyop
chown -R psyop:psyop /opt/psyop

echo "[5/6] Creating log directory"
mkdir -p /var/log/psyop
chown -R psyop:psyop /var/log/psyop

echo "[6/6] Bootstrap complete"
REMOTE_BOOTSTRAP

echo "Syncing repository to remote ${REMOTE_HOST}:${APP_DIR}"
rsync -az --delete --exclude .stack-work --exclude .git ./ ${REMOTE_USER}@${REMOTE_HOST}:${APP_DIR}/

echo "Preparing asset directories on remote"
ssh ${REMOTE_USER}@${REMOTE_HOST} bash -s <<'REMOTE_ASSETS'
set -euo pipefail
cd /opt/psyop
# Ensure output directory exists
mkdir -p Public/Assets
# Copy hero asset and known icons if present in content
if [[ -f Private/Content/Site.yaml ]]; then
  HERO=$(awk -F '"' '/hero_image:/ {print $2}' Private/Content/Site.yaml || true)
  if [[ -n "${HERO:-}" ]]; then
    HPATH=${HERO#/}
    SRC="Private/Assets/${HPATH#assets/}"
    DST="Public/Assets/${HPATH#assets/}"
    if [[ -f "$SRC" ]]; then
      install -D "$SRC" "$DST"
      command -v jpegoptim >/dev/null && [[ "$DST" =~ \.jpe?g$ ]] && jpegoptim --strip-all --max=85 "$DST" || true
      command -v optipng   >/dev/null && [[ "$DST" =~ \.png$   ]] && optipng -o2 "$DST" || true
      command -v svgcleaner>//dev/null && [[ "$DST" =~ \.svg$   ]] && svgcleaner "$DST" "$DST" || true
    fi
  fi
fi
REMOTE_ASSETS

echo "Building on remote"
ssh ${REMOTE_USER}@${REMOTE_HOST} bash -s <<REMOTE_BUILD
set -euo pipefail
cd ${APP_DIR}
mkdir -p ${APP_DIR}/.stack ${APP_DIR}/bin
chown -R ${APP_USER}:${APP_USER} ${APP_DIR}
sudo -u ${APP_USER} STACK_ROOT=${APP_DIR}/.stack stack setup
sudo -u ${APP_USER} STACK_ROOT=${APP_DIR}/.stack stack build --copy-bins --local-bin-path ${APP_DIR}/bin
REMOTE_BUILD

echo "Installing systemd service"
ssh ${REMOTE_USER}@${REMOTE_HOST} bash -s <<'REMOTE_SERVICE'
set -euo pipefail
cat >/etc/systemd/system/psyop-website.service <<SERVICE
[Unit]
Description=PSYOP Website (Warp TLS)
After=network.target

[Service]
User=psyop
Group=psyop
WorkingDirectory=/opt/psyop
Environment=PORT=443
Environment=ENVIRONMENT=production
Environment=HTTPS_ENABLE=true
Environment=WWW_CANONICAL=true
Environment=REDIRECT_HTTPS=false
Environment=CERT_FILE=/etc/ssl/certs/psyop.crt
Environment=KEY_FILE=/etc/ssl/private/psyop.key
ExecStart=/opt/psyop/bin/psyop-website-exe
Restart=on-failure
RestartSec=5
StandardOutput=append:/var/log/psyop/server.log
StandardError=append:/var/log/psyop/server.log
AmbientCapabilities=CAP_NET_BIND_SERVICE
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
NoNewPrivileges=true

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable psyop-website.service
systemctl restart psyop-website.service
systemctl status --no-pager psyop-website.service || true
REMOTE_SERVICE

echo "Setting cap_net_bind_service on binary for privileged ports"
ssh ${REMOTE_USER}@${REMOTE_HOST} bash -s <<'REMOTE_SETCAP'
set -euo pipefail
if command -v setcap >/dev/null 2>&1; then
  setcap 'cap_net_bind_service=+ep' /opt/psyop/bin/psyop-website-exe || true
fi
REMOTE_SETCAP

if [[ "${INSTALL_HTTP_REDIRECT:-false}" == "true" ]]; then
  echo "Installing HTTP->HTTPS redirect unit on port 80"
  ssh ${REMOTE_USER}@${REMOTE_HOST} bash -s <<'REDIRECT_UNIT'
set -euo pipefail
cat >/etc/systemd/system/psyop-website-redirect.service <<SERVICE
[Unit]
Description=PSYOP HTTP Redirect (Warp)
After=network.target

[Service]
User=psyop
Group=psyop
WorkingDirectory=/opt/psyop
Environment=PORT=80
Environment=ENVIRONMENT=production
Environment=HTTPS_ENABLE=false
Environment=REDIRECT_HTTPS=true
Environment=WWW_CANONICAL=true
ExecStart=/opt/psyop/bin/psyop-website-exe
Restart=on-failure
RestartSec=3
StandardOutput=append:/var/log/psyop/server.log
StandardError=append:/var/log/psyop/server.log
AmbientCapabilities=CAP_NET_BIND_SERVICE
CapabilityBoundingSet=CAP_NET_BIND_SERVICE
NoNewPrivileges=true

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable psyop-website-redirect.service
systemctl restart psyop-website-redirect.service
systemctl status --no-pager psyop-website-redirect.service || true
REDIRECT_UNIT
fi

echo "Running remote smoke tests"
HOST=https://${REMOTE_HOST} bash -c '"$(dirname "$0")"/smoke-tests.sh'

echo "Deployment finished. Visit https://${REMOTE_HOST}"
}

# Main execution
main() {
    echo "🚀 Starting PSYOP deployment..."
    echo "   Mode: ${DEPLOYMENT_MODE}"
    echo "   Branch: ${BRANCH}"
    if [ -n "$REMOTE_HOST" ]; then
        echo "   Target: ${REMOTE_USER}@${REMOTE_HOST}"
    fi
    echo
    
    check_prerequisites
    
    case "$DEPLOYMENT_MODE" in
        realtime)
            setup_realtime_deployment
            ;;
        auto)
            run_automated_deployment
            ;;
        manual)
            run_manual_deployment
            ;;
        *)
            print_error "Unknown deployment mode: $DEPLOYMENT_MODE"
            show_usage
            exit 1
            ;;
    esac
}

# Run main function
main "$@"

