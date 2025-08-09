#!/bin/bash

# psyop.ca Kubernetes Deployment Script
# This script handles the complete deployment process to DigitalOcean Kubernetes

set -e

# Configuration
IMAGE_NAME="psyop/psyop-website"
IMAGE_TAG="${1:-latest}"
REGISTRY="${REGISTRY:-registry.digitalocean.com/psyop}"
FULL_IMAGE="${REGISTRY}/${IMAGE_NAME}:${IMAGE_TAG}"

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

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."
    
    # Check if docker is available
    if ! command -v docker &> /dev/null; then
        error "Docker is not installed or not in PATH"
    fi
    
    # Check if docker daemon is running
    if ! docker info &> /dev/null; then
        error "Docker daemon is not running. Please start Docker Desktop or Docker daemon."
    fi
    
    # Check if kubectl is available
    if ! command -v kubectl &> /dev/null; then
        error "kubectl is not installed. Run: brew install kubectl"
    fi
    
    # Check if doctl is available (DigitalOcean CLI)
    if ! command -v doctl &> /dev/null; then
        warn "doctl is not installed. Install with: brew install doctl"
        warn "You'll need doctl to manage DigitalOcean resources"
    fi
    
    success "Prerequisites check completed"
}

# Build Docker image
build_image() {
    log "Building Docker image: ${FULL_IMAGE}"
    
    # Build the image
    docker build -t "${IMAGE_NAME}:${IMAGE_TAG}" .
    
    # Tag for registry
    docker tag "${IMAGE_NAME}:${IMAGE_TAG}" "${FULL_IMAGE}"
    
    success "Image built successfully: ${FULL_IMAGE}"
}

# Push to registry
push_image() {
    log "Pushing image to registry: ${FULL_IMAGE}"
    
    # Login to DigitalOcean registry (requires doctl auth)
    if command -v doctl &> /dev/null; then
        doctl registry login
    else
        warn "doctl not found. You may need to login to your registry manually:"
        warn "docker login registry.digitalocean.com"
    fi
    
    # Push the image
    docker push "${FULL_IMAGE}"
    
    success "Image pushed successfully"
}

# Update Kubernetes manifests with new image
update_manifests() {
    log "Updating Kubernetes manifests with image: ${FULL_IMAGE}"
    
    # Update deployment.yaml
    sed -i.bak "s|image: psyop/psyop-website:.*|image: ${FULL_IMAGE}|g" k8s/deployment.yaml
    
    success "Manifests updated"
}

# Deploy to Kubernetes
deploy_to_k8s() {
    log "Deploying to Kubernetes cluster..."
    
    # Check if cluster is accessible
    if ! kubectl cluster-info &> /dev/null; then
        error "Cannot connect to Kubernetes cluster. Please check your kubeconfig."
    fi
    
    # Apply manifests in order
    log "Applying Kubernetes manifests..."
    kubectl apply -f k8s/deployment.yaml
    kubectl apply -f k8s/service.yaml
    kubectl apply -f k8s/ingress.yaml
    
    success "Deployment manifests applied"
}

# Verify deployment
verify_deployment() {
    log "Verifying deployment..."
    
    # Wait for deployment to be ready
    kubectl rollout status deployment/psyop-website --timeout=300s
    
    # Get deployment status
    kubectl get pods -l app=psyop-website
    kubectl get services -l app=psyop-website
    kubectl get ingress
    
    success "Deployment verification completed"
}

# Main deployment function
main() {
    log "Starting deployment process for psyop.ca website"
    log "Image: ${FULL_IMAGE}"
    
    check_prerequisites
    build_image
    push_image
    update_manifests
    deploy_to_k8s
    verify_deployment
    
    success "Deployment completed successfully!"
    
    log "Your website should be available at:"
    log "- https://psyop.ca"
    log "- https://www.psyop.ca"
    
    log "To check deployment status:"
    log "kubectl get pods -l app=psyop-website"
    log "kubectl logs -l app=psyop-website"
}

# Show help
show_help() {
    echo "Usage: $0 [IMAGE_TAG]"
    echo ""
    echo "Deploy psyop.ca website to Kubernetes"
    echo ""
    echo "Arguments:"
    echo "  IMAGE_TAG    Docker image tag (default: latest)"
    echo ""
    echo "Environment variables:"
    echo "  REGISTRY     Container registry URL (default: registry.digitalocean.com/psyop)"
    echo ""
    echo "Examples:"
    echo "  $0                    # Deploy with 'latest' tag"
    echo "  $0 v1.0.0             # Deploy with 'v1.0.0' tag"
    echo "  REGISTRY=my-registry.com $0 v1.0.0"
    echo ""
    echo "Prerequisites:"
    echo "  - Docker daemon running"
    echo "  - kubectl configured for your cluster"
    echo "  - doctl configured for DigitalOcean (optional)"
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
