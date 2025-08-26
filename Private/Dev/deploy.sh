#!/usr/bin/env bash
set -euo pipefail

# PSYOP deployment helper: run this locally to bootstrap and deploy to a remote host

REMOTE_HOST=${REMOTE_HOST:-}
REMOTE_USER=${REMOTE_USER:-root}
APP_USER=${APP_USER:-psyop}
APP_DIR=${APP_DIR:-/opt/psyop}
SERVICE_NAME=${SERVICE_NAME:-psyop-website}
BRANCH=${BRANCH:-main}

if [[ -z "${REMOTE_HOST}" ]]; then
  echo "REMOTE_HOST is required (e.g. REMOTE_HOST=147.182.144.112)" >&2
  exit 1
fi

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
      command -v svgcleaner>/dev/null && [[ "$DST" =~ \.svg$   ]] && svgcleaner "$DST" "$DST" || true
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
HOST=http://${REMOTE_HOST}:8080 bash -c '"$(dirname "$0")"/Private/Dev/smoke-tests.sh'

echo "Deployment finished. Visit http://${REMOTE_HOST}:8080"

