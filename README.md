# PSYOP Official Website

[![License: BSD-3-Clause](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Haskell](https://img.shields.io/badge/language-Haskell-5e5086.svg)](https://www.haskell.org/)
[![Docker](https://img.shields.io/badge/docker-enabled-blue.svg)](https://www.docker.com/)
[![Kubernetes](https://img.shields.io/badge/k8s-ready-326ce5.svg)](https://kubernetes.io/)

Official website for **PSYOP**, an experimental electronic music project exploring the intersection of technology, consciousness, and sound.

🌐 **Live Website:** [https://psyop.ca](https://psyop.ca)

## 🎵 About PSYOP

PSYOP is an experimental electronic music project that creates immersive audio experiences challenging conventional boundaries. Our music delves into cyberpunk aesthetics and futuristic soundscapes, transporting listeners to alternate realities where human and machine consciousness intersect.

## 🏗️ Architecture

This website is built with modern, scalable technologies:

- **Backend:** Haskell with Servant web framework
- **Frontend:** Server-side rendered HTML with Blaze templating
- **Containerization:** Docker with multi-stage builds
- **Orchestration:** Kubernetes deployment ready
- **Reverse Proxy:** Nginx with SSL/TLS termination
- **Infrastructure:** DigitalOcean deployment with automated CI/CD

## 🛠️ Tech Stack

### Core Technologies
- **[Haskell](https://www.haskell.org/)** - Type-safe functional programming
- **[Servant](https://docs.servant.dev/)** - Type-safe web API framework
- **[Warp](https://hackage.haskell.org/package/warp)** - High-performance HTTP server
- **[Blaze HTML](https://hackage.haskell.org/package/blaze-html)** - HTML templating library

### DevOps & Deployment
- **[Docker](https://www.docker.com/)** - Containerization
- **[Kubernetes](https://kubernetes.io/)** - Container orchestration
- **[Nginx](https://nginx.org/)** - Reverse proxy and static file serving
- **[Let's Encrypt](https://letsencrypt.org/)** - SSL certificate automation
- **[Stack](https://docs.haskellstack.org/)** - Haskell build tool

### Development Tools
- **[Sharp](https://sharp.pixelplumbing.com/)** - Image optimization
- **[HSpec](https://hspec.github.io/)** - Testing framework

## 🚀 Quick Start

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (Haskell build tool)
- [Docker](https://docs.docker.com/get-docker/) (for containerization)
- [Node.js](https://nodejs.org/) (for image optimization scripts)

### Local Development

1. **Clone the repository:**
   ```bash
   git clone https://github.com/Shahzebqazi/psyop.ca.git
   cd psyop.ca
   ```

2. **Install dependencies:**
   ```bash
   stack setup
   stack build --dependencies-only
   ```

3. **Build and run:**
   ```bash
   stack build
   stack run
   ```

4. **Visit the website:**
   ```
   http://localhost:8080
   ```

### Available Routes

- `/` - Homepage with latest releases
- `/about` - About PSYOP and the project
- `/contact` - Contact information and social links
- `/links` - Music platforms and streaming services
- `/admin` - Admin panel (future feature)
- `/static/*` - Static assets (CSS, images, etc.)

## 🐳 Docker Deployment

### Build Docker Image

```bash
docker build -t psyop-website:latest .
```

### Run Container

```bash
docker run -d \
  --name psyop-website \
  --restart unless-stopped \
  -p 8080:8080 \
  psyop-website:latest
```

## ☸️ Kubernetes Deployment

Deploy to Kubernetes cluster:

```bash
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
kubectl apply -f k8s/ingress.yaml
```

## 🌐 Production Deployment

### Automated Server Deployment

Use the provided deployment script for DigitalOcean:

```bash
# Full deployment (server setup + code deployment)
./deploy-server.sh

# Options for specific tasks
./deploy-server.sh --setup-only    # Only setup server environment
./deploy-server.sh --code-only     # Only deploy code
./deploy-server.sh --verify-only   # Only verify deployment
```

### Manual Deployment Steps

1. **Server Setup:**
   - Ubuntu 22.04 LTS server
   - Docker and Docker Compose installation
   - Nginx reverse proxy configuration
   - SSL certificate via Let's Encrypt

2. **Application Deployment:**
   - Git repository cloning/updating
   - Docker image building
   - Container orchestration
   - Health checks and monitoring

## 🧪 Testing

Run the test suite:

```bash
# Run all tests
stack test

# Run with coverage
stack test --coverage

# Run specific test module
stack test psyop-website:test:psyop-website-test
```

## 📁 Project Structure

```
psyop.ca/
├── app/                    # Application entry point
│   └── Main.hs            # Main executable
├── src/                    # Source code
│   └── Lib.hs             # Core application logic
├── test/                   # Test suites
│   └── Spec.hs            # Test specifications
├── static/                 # Static assets
│   ├── style.css          # Stylesheets
│   └── *.jpg              # Images and media
├── k8s/                    # Kubernetes manifests
│   ├── deployment.yaml    # Application deployment
│   ├── service.yaml       # Service configuration
│   └── ingress.yaml       # Ingress configuration
├── scripts/                # Build and utility scripts
│   └── optimize-images.js # Image optimization
├── config/                 # Configuration files
│   └── nginx.conf         # Nginx configuration
├── docker/                 # Docker configurations
├── Dockerfile             # Multi-stage Docker build
├── deploy-server.sh       # Server deployment script
├── git_rollback.sh        # Rollback script
├── stack.yaml             # Stack configuration
├── package.yaml           # Haskell package configuration
└── README.md              # This file
```

## 🎨 Design & Features

### Current Features
- **Responsive Design:** Mobile-first approach with modern CSS
- **Performance Optimized:** Lightweight Haskell backend
- **SEO Ready:** Server-side rendering with proper meta tags
- **Security Headers:** HTTPS with security headers configured
- **Health Monitoring:** Built-in health checks and logging

### Upcoming Features
- **Music Integration:** Spotify/Bandcamp API integration
- **Tour Dates:** Dynamic event listing
- **Newsletter:** Mailchimp integration
- **Social Media:** Dynamic social feed integration
- **Admin Panel:** Content management system

## 🔧 Development

### Image Optimization

Optimize images for web delivery (requires Node.js):

```bash
# Install sharp dependency first
npm install sharp

# Run optimization script
node scripts/optimize-images.js
```

### Code Style

The project follows Haskell best practices with strict GHC warnings enabled:

- Wall warnings
- Type safety checks
- Incomplete pattern warnings
- Export list requirements

## 🚨 Monitoring & Logs

### Container Logs
```bash
# View application logs
docker logs psyop-website

# Follow logs in real-time
docker logs -f psyop-website
```

### Health Checks
- **Application:** `curl http://localhost:8080/`
- **Container:** Built-in Docker health check
- **Kubernetes:** Liveness and readiness probes

## 🔄 Rollback

In case of deployment issues:

```bash
# Automated rollback script
./git_rollback.sh

# Manual rollback
git reset --hard HEAD^
docker restart psyop-website
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the BSD 3-Clause License - see the [LICENSE](LICENSE) file for details.

## 📞 Contact

- **Website:** [https://psyop.ca](https://psyop.ca)
- **Email:** [admin@psyop.ca](mailto:admin@psyop.ca)
- **Developer:** [github.com/Shahzebqazi](https://github.com/Shahzebqazi)

## 🙏 Acknowledgments

- Built with [Haskell](https://www.haskell.org/) and [Servant](https://docs.servant.dev/)
- Deployed on [DigitalOcean](https://www.digitalocean.com/)
- SSL certificates by [Let's Encrypt](https://letsencrypt.org/)
- Containerized with [Docker](https://www.docker.com/)

---

**PSYOP** - *Pushing the boundaries of electronic music*