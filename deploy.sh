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
        error "Docker is not installed or not in PATH. Install from: https://docs.docker.com/get-docker/"
    fi
    
    # Check if docker daemon is running
    if ! timeout 10 docker info &> /dev/null; then
        error "Docker daemon is not running or not responding. Please start Docker Desktop or Docker daemon."
    fi
    
    # Check if kubectl is available
    if ! command -v kubectl &> /dev/null; then
        error "kubectl is not installed. Install with: brew install kubectl"
    fi
    
    # Check kubectl connectivity with timeout
    if ! timeout 10 kubectl cluster-info &> /dev/null; then
        warn "Cannot connect to Kubernetes cluster. Continuing with local Docker build only."
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
    
    # Check available disk space
    AVAILABLE_SPACE=$(df . | awk 'NR==2 {print $4}')
    if [ "$AVAILABLE_SPACE" -lt 2000000 ]; then
        warn "Low disk space detected. Docker build may fail."
    fi
    
    # Build the image with timeout
    if ! timeout 1200 docker build -t "${IMAGE_NAME}:${IMAGE_TAG}" .; then
        error "Docker build failed or timed out after 20 minutes"
    fi
    
    # Tag for registry
    if ! docker tag "${IMAGE_NAME}:${IMAGE_TAG}" "${FULL_IMAGE}"; then
        error "Failed to tag image for registry"
    fi
    
    success "Image built successfully: ${FULL_IMAGE}"
}

# Push to registry
push_image() {
    log "Pushing image to registry: ${FULL_IMAGE}"
    
    # Login to DigitalOcean registry (requires doctl auth)
    if command -v doctl &> /dev/null; then
        if ! timeout 60 doctl registry login; then
            error "Failed to login to DigitalOcean registry"
        fi
    else
        warn "doctl not found. You may need to login to your registry manually:"
        warn "docker login registry.digitalocean.com"
        return 0
    fi
    
    # Check if image exists locally
    if ! docker image inspect "${FULL_IMAGE}" &> /dev/null; then
        error "Image ${FULL_IMAGE} not found locally. Build the image first."
    fi
    
    # Push the image with retry logic
    for i in {1..3}; do
        if timeout 600 docker push "${FULL_IMAGE}"; then
            success "Image pushed successfully"
            return 0
        else
            warn "Push attempt $i failed, retrying in 10 seconds..."
            sleep 10
        fi
    done
    
    error "Failed to push image after 3 attempts"
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
    if ! timeout 30 kubectl cluster-info &> /dev/null; then
        error "Cannot connect to Kubernetes cluster. Please check your kubeconfig."
    fi
    
    # Validate manifests before applying
    log "Validating Kubernetes manifests..."
    for manifest in k8s/deployment.yaml k8s/service.yaml k8s/ingress.yaml; do
        if ! kubectl apply --dry-run=client -f "$manifest" &> /dev/null; then
            error "Invalid manifest: $manifest"
        fi
    done
    
    # Apply manifests in order with error checking
    log "Applying Kubernetes manifests..."
    if ! kubectl apply -f k8s/deployment.yaml; then
        error "Failed to apply deployment manifest"
    fi
    
    if ! kubectl apply -f k8s/service.yaml; then
        error "Failed to apply service manifest"
    fi
    
    if ! kubectl apply -f k8s/ingress.yaml; then
        error "Failed to apply ingress manifest"
    fi
    
    success "Deployment manifests applied"
}

# Verify deployment
verify_deployment() {
    log "Verifying deployment..."
    
    # Wait for deployment to be ready with timeout
    if ! timeout 300 kubectl rollout status deployment/psyop-website --timeout=300s; then
        error "Deployment failed to become ready within 5 minutes"
    fi
    
    # Get deployment status
    log "Checking pod status..."
    kubectl get pods -l app=psyop-website
    
    log "Checking service status..."
    kubectl get services -l app=psyop-website
    
    log "Checking ingress status..."
    kubectl get ingress
    
    # Check if pods are actually running
    RUNNING_PODS=$(kubectl get pods -l app=psyop-website --field-selector=status.phase=Running --no-headers | wc -l)
    if [ "$RUNNING_PODS" -eq 0 ]; then
        error "No running pods found for psyop-website"
    fi
    
    success "Deployment verification completed - $RUNNING_PODS pods running"
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
