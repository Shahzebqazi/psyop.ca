#!/bin/bash

# DigitalOcean Kubernetes Setup Script for psyop.ca
# This script helps set up the necessary infrastructure for deploying to DO K8s

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

# Install required tools
install_tools() {
    log "Installing required tools..."
    
    # Install doctl (DigitalOcean CLI)
    if ! command -v doctl &> /dev/null; then
        log "Installing doctl..."
        brew install doctl
    else
        success "doctl already installed"
    fi
    
    # Check kubectl
    if ! command -v kubectl &> /dev/null; then
        log "Installing kubectl..."
        brew install kubectl
    else
        success "kubectl already installed"
    fi
    
    # Check Docker
    if ! command -v docker &> /dev/null; then
        log "Installing Docker..."
        brew install --cask docker
        warn "Please start Docker Desktop after installation"
    else
        success "Docker already installed"
    fi
    
    success "All tools installed"
}

# Configure DigitalOcean authentication
setup_do_auth() {
    log "Setting up DigitalOcean authentication..."
    
    echo "You'll need a DigitalOcean API token."
    echo "Get one from: https://cloud.digitalocean.com/account/api/tokens"
    echo ""
    read -p "Enter your DigitalOcean API token: " -s DO_TOKEN
    echo ""
    
    # Authenticate with doctl
    doctl auth init -t "$DO_TOKEN"
    
    # Test authentication
    doctl account get
    
    success "DigitalOcean authentication configured"
}

# Create container registry
create_registry() {
    log "Creating DigitalOcean Container Registry..."
    
    # Check if registry exists
    if doctl registry get psyop &> /dev/null; then
        success "Registry 'psyop' already exists"
    else
        # Create registry
        doctl registry create psyop --region nyc3
        success "Container registry 'psyop' created"
    fi
    
    # Login to registry
    doctl registry login
    success "Logged into container registry"
}

# Create or configure Kubernetes cluster
setup_k8s_cluster() {
    log "Setting up Kubernetes cluster..."
    
    echo "Choose an option:"
    echo "1. Create a new DigitalOcean Kubernetes cluster"
    echo "2. Use existing cluster"
    echo "3. Skip cluster setup"
    read -p "Enter your choice (1-3): " choice
    
    case $choice in
        1)
            create_new_cluster
            ;;
        2)
            configure_existing_cluster
            ;;
        3)
            warn "Skipping cluster setup"
            ;;
        *)
            error "Invalid choice"
            ;;
    esac
}

# Create new K8s cluster
create_new_cluster() {
    log "Creating new Kubernetes cluster..."
    
    # Get available regions
    echo "Available regions:"
    doctl kubernetes options regions
    echo ""
    read -p "Enter region (e.g., nyc3): " REGION
    
    # Get available node sizes
    echo "Available node sizes:"
    doctl kubernetes options sizes
    echo ""
    read -p "Enter node size (e.g., s-2vcpu-2gb): " NODE_SIZE
    
    read -p "Enter cluster name (default: psyop-k8s): " CLUSTER_NAME
    CLUSTER_NAME=${CLUSTER_NAME:-psyop-k8s}
    
    read -p "Enter number of nodes (default: 2): " NODE_COUNT
    NODE_COUNT=${NODE_COUNT:-2}
    
    # Create cluster
    log "Creating cluster '$CLUSTER_NAME' in region '$REGION'..."
    doctl kubernetes cluster create "$CLUSTER_NAME" \
        --region "$REGION" \
        --node-pool "name=worker-pool;size=$NODE_SIZE;count=$NODE_COUNT" \
        --wait
    
    # Get kubeconfig
    doctl kubernetes cluster kubeconfig save "$CLUSTER_NAME"
    
    success "Kubernetes cluster created and configured"
}

# Configure existing cluster
configure_existing_cluster() {
    log "Available Kubernetes clusters:"
    doctl kubernetes cluster list
    echo ""
    
    read -p "Enter cluster ID or name: " CLUSTER_ID
    
    # Get kubeconfig
    doctl kubernetes cluster kubeconfig save "$CLUSTER_ID"
    
    success "Kubeconfig updated for existing cluster"
}

# Install ingress controller
setup_ingress() {
    log "Setting up NGINX Ingress Controller..."
    
    # Check if kubectl can connect
    if ! kubectl cluster-info &> /dev/null; then
        error "Cannot connect to Kubernetes cluster"
    fi
    
    # Install NGINX Ingress Controller
    kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/controller-v1.8.2/deploy/static/provider/do/deploy.yaml
    
    # Wait for ingress controller to be ready
    log "Waiting for ingress controller to be ready..."
    kubectl wait --namespace ingress-nginx \
        --for=condition=ready pod \
        --selector=app.kubernetes.io/component=controller \
        --timeout=300s
    
    success "NGINX Ingress Controller installed"
}

# Install cert-manager for SSL
setup_cert_manager() {
    log "Setting up cert-manager for SSL certificates..."
    
    # Install cert-manager
    kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.13.2/cert-manager.yaml
    
    # Wait for cert-manager to be ready
    log "Waiting for cert-manager to be ready..."
    kubectl wait --namespace cert-manager \
        --for=condition=ready pod \
        --selector=app.kubernetes.io/component=controller \
        --timeout=300s
    
    # Create ClusterIssuer for Let's Encrypt
    cat <<EOF | kubectl apply -f -
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: admin@psyop.ca
    privateKeySecretRef:
      name: letsencrypt-prod
    solvers:
    - http01:
        ingress:
          class: nginx
EOF
    
    success "cert-manager installed and configured"
}

# Verify setup
verify_setup() {
    log "Verifying setup..."
    
    # Check cluster connection
    kubectl cluster-info
    
    # Check nodes
    kubectl get nodes
    
    # Check ingress controller
    kubectl get pods -n ingress-nginx
    
    # Check cert-manager
    kubectl get pods -n cert-manager
    
    success "Setup verification completed"
}

# Main setup function
main() {
    log "Starting DigitalOcean Kubernetes setup for psyop.ca"
    
    install_tools
    setup_do_auth
    create_registry
    setup_k8s_cluster
    
    # Only setup ingress and cert-manager if we have a cluster
    if kubectl cluster-info &> /dev/null; then
        setup_ingress
        setup_cert_manager
        verify_setup
    fi
    
    success "Setup completed successfully!"
    
    echo ""
    log "Next steps:"
    log "1. Update DNS records to point to your cluster's load balancer IP"
    log "2. Run './deploy.sh' to deploy your website"
    
    # Get load balancer IP
    if kubectl cluster-info &> /dev/null; then
        log "Getting load balancer IP..."
        kubectl get services -n ingress-nginx | grep LoadBalancer
    fi
}

# Show help
show_help() {
    echo "Usage: $0"
    echo ""
    echo "Set up DigitalOcean Kubernetes infrastructure for psyop.ca"
    echo ""
    echo "This script will:"
    echo "  - Install required tools (doctl, kubectl, docker)"
    echo "  - Configure DigitalOcean authentication"
    echo "  - Create container registry"
    echo "  - Set up Kubernetes cluster"
    echo "  - Install NGINX Ingress Controller"
    echo "  - Install cert-manager for SSL"
    echo ""
    echo "Prerequisites:"
    echo "  - DigitalOcean account and API token"
    echo "  - Homebrew package manager"
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
