# PSYOP Official Website

Official website for PSYOP band built with modern web technologies and deployed using GitOps practices.

## Architecture

- **Frontend**: Static HTML/CSS/Sass
- **Server**: Nginx in Docker container
- **Deployment**: Kubernetes with GitOps (Flux)
- **CI/CD**: GitHub Actions
- **Infrastructure**: Digital Ocean

## Project Structure

```
psyop.ca/
├── src/                    # Source code
│   ├── index.html         # Main HTML file
│   ├── styles/            # Sass stylesheets
│   └── assets/            # Images, audio, other assets
├── k8s/                   # Kubernetes manifests
│   ├── deployment.yaml    # Application deployment
│   ├── service.yaml       # Service definition
│   └── ingress.yaml       # Ingress configuration
├── config/                # Configuration files
│   └── nginx.conf         # Nginx configuration
├── scripts/               # Build scripts
│   └── build.js           # Build automation
├── .github/workflows/     # CI/CD pipelines
│   └── deploy.yml         # Main deployment workflow
├── Dockerfile             # Container definition
└── package.json           # Node.js dependencies
```

## Getting Started

### Prerequisites

- Node.js 18+
- Docker
- kubectl (for Kubernetes deployment)
- Access to Digital Ocean cluster

### Local Development

1. **Install dependencies:**
   ```bash
   npm install
   ```

2. **Build the website:**
   ```bash
   npm run build
   ```

3. **Start development server:**
   ```bash
   npm run dev
   ```

4. **Access the website:**
   Open http://localhost:8000 in your browser

### Building for Production

```bash
# Clean previous builds
npm run clean

# Build CSS and assets
npm run build

# Build Docker image
docker build -t psyop-website .

# Test the container locally
docker run -p 8080:8080 psyop-website
```

## Deployment

### GitHub Actions Setup

1. **Create Docker Hub account** and get access token
2. **Add GitHub Secrets:**
   - `DOCKERHUB_USERNAME`: Your Docker Hub username
   - `DOCKERHUB_TOKEN`: Your Docker Hub access token

3. **Update deployment configuration:**
   - Edit `k8s/deployment.yaml` to use your Docker Hub image
   - Update `.github/workflows/deploy.yml` with your image name

### Kubernetes Deployment

1. **Apply Kubernetes manifests:**
   ```bash
   kubectl apply -f k8s/
   ```

2. **Verify deployment:**
   ```bash
   kubectl get pods
   kubectl get services
   kubectl get ingress
   ```

### GitOps with Flux

The deployment uses GitOps principles where:
- Code changes trigger CI/CD pipeline
- Docker images are built and pushed to registry
- Kubernetes manifests are updated with new image tags
- Flux automatically deploys changes to the cluster

## Integrations

### External Services

- **Mailchimp**: Newsletter signup integration
- **Spotify**: Music streaming links
- **Bandcamp**: Direct music sales
- **DistroKid**: Distribution platform links

### SSL/TLS

HTTPS is handled by cert-manager with Let's Encrypt:
- Automatic certificate provisioning
- Certificate renewal
- Configured in `k8s/ingress.yaml`

## Monitoring and Maintenance

### Health Checks

- Application health endpoint: `/health`
- Kubernetes liveness and readiness probes configured
- Nginx access and error logs available

### Rollback Procedures

**Using GitOps (Recommended):**
1. Revert the commit in git
2. Push changes
3. Flux will automatically deploy the previous version

**Manual rollback:**
```bash
./git_rollback.sh
```

## Development Workflow

1. **Make changes** to source code in `src/`
2. **Test locally** with `npm run dev`
3. **Commit and push** to main branch
4. **GitHub Actions** automatically:
   - Runs tests
   - Builds Docker image
   - Updates Kubernetes manifests
   - Deploys to production

## Content Management

### Adding New Music

1. Update `src/index.html` with new tracks
2. Add streaming platform links
3. Upload audio samples to `src/assets/audio/`

### Tour Dates

1. Update the tours section in `src/index.html`
2. Consider integrating with external calendar APIs

### Social Media

1. Update social links in the contact section
2. Ensure all links are tested in the CI/CD pipeline

## Security

- Container runs as non-root user
- Nginx security headers configured
- HTTPS enforced
- Resource limits set in Kubernetes

## Support

- **Website Issues**: admin@psyop.ca
- **Development**: dev@psyop.ca
- **Technical Documentation**: See `DeploymentPlan` file

## License

Copyright 2024 PSYOP. All rights reserved.
