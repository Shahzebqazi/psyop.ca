# PSYOP Website - Real-Time Deployment Guide

This guide explains how to set up automatic, real-time website updates when you push code to the main branch.

## 🚀 How It Works

The system consists of several components that work together to provide real-time deployment:

1. **GitHub Webhook** → Triggers deployment when code is pushed
2. **Webhook Server** → Receives webhook and manages deployment
3. **Auto-Deploy Script** → Handles the actual deployment process
4. **Systemd Services** → Keep everything running automatically

## 📋 Prerequisites

- DigitalOcean droplet (Ubuntu 22.04+ recommended)
- Domain name pointing to your server
- SSH access to your server
- GitHub repository with your website code

## 🛠️ Server Setup

### 1. Initial Server Preparation

```bash
# SSH into your server
ssh root@your-server-ip

# Run the server setup script
curl -sSL https://raw.githubusercontent.com/${GITHUB_USERNAME:-Shahzebqazi}/${GITHUB_REPO:-psyop.ca}/main/server-setup.sh | bash
```

### 2. Clone and Setup Repository

```bash
# Clone your repository
git clone https://github.com/${GITHUB_USERNAME:-Shahzebqazi}/${GITHUB_REPO:-psyop.ca}.git /opt/psyop-website
cd /opt/psyop-website

# Make scripts executable
chmod +x auto-deploy.sh
chmod +x deploy-digitalocean.sh
```

### 3. Run Auto-Deployment Setup

```bash
# Run the full setup (recommended for first time)
./auto-deploy.sh

# Or run setup only
./auto-deploy.sh setup
```

## 🔧 Configuration

### Webhook Secret

The system generates a webhook secret automatically. You'll find it in `/etc/environment`:

```bash
cat /etc/environment | grep WEBHOOK_SECRET
```

### GitHub Webhook Setup

1. Go to your GitHub repository → Settings → Webhooks
2. Click "Add webhook"
3. Set the following:
   - **Payload URL**: `http://your-server-ip:9000`
   - **Content type**: `application/json`
   - **Secret**: Use the webhook secret from `/etc/environment`
   - **Events**: Select "Just the push event"
   - **Active**: ✅ Checked

## 📁 Core Scripts Overview

### `auto-deploy.sh` - Main Deployment Script
- **Purpose**: Handles the complete deployment pipeline
- **Usage**: `./auto-deploy.sh [deploy|status|setup|help]`
- **Features**:
  - Initializes deployment environment
  - Sets up systemd services
  - Configures nginx
  - Manages webhook server
  - Deploys the application

### `webhook-server.py` - Webhook Receiver
- **Purpose**: Listens for GitHub webhooks and triggers deployments
- **Port**: 9000 (configurable)
- **Features**:
  - Verifies webhook signatures
  - Only triggers on main branch pushes
  - Runs deployments in background
  - Comprehensive logging

### `server-setup.sh` - Server Preparation
- **Purpose**: Prepares the server environment
- **Usage**: Run once on a fresh server
- **Features**:
  - Installs required packages
  - Configures firewall
  - Sets up nginx
  - Creates systemd services

## 🔄 Deployment Flow

```
1. You push code to main branch
   ↓
2. GitHub sends webhook to your server
   ↓
3. Webhook server receives and verifies request
   ↓
4. Auto-deploy script pulls latest code
   ↓
5. Haskell application is rebuilt
   ↓
6. Service is restarted
   ↓
7. Website is updated (usually within 30 seconds)
```

## 📊 Monitoring and Status

### Check Deployment Status

```bash
# Show overall status
./auto-deploy.sh status

# Check individual services
sudo systemctl status psyop-website
sudo systemctl status psyop-webhook
sudo systemctl status nginx
```

### View Logs

```bash
# Application logs
sudo journalctl -u psyop-website -f

# Webhook server logs
sudo journalctl -u psyop-webhook -f

# Deployment logs
tail -f /var/log/psyop-deploy.log

# Nginx logs
sudo tail -f /var/log/nginx/access.log
```

## 🚨 Troubleshooting

### Common Issues

1. **Webhook not triggering**
   - Check webhook secret matches
   - Verify server firewall allows port 9000
   - Check webhook server logs

2. **Build failures**
   - Ensure Haskell Stack is installed
   - Check build logs in `/var/log/psyop-deploy.log`
   - Verify all dependencies are available

3. **Service not starting**
   - Check systemd service status
   - Verify file permissions
   - Check application logs

### Debug Commands

```bash
# Test webhook manually
curl -X POST http://localhost:9000 \
  -H "Content-Type: application/json" \
  -H "X-Hub-Signature-256: sha256=test" \
  -d '{"ref":"refs/heads/main"}'

# Check port usage
sudo netstat -tlnp | grep -E ':(8080|9000)'

# Test nginx configuration
sudo nginx -t

# Check file permissions
ls -la /opt/psyop-website/
```

## 🔒 Security Considerations

- Webhook secret is automatically generated and stored securely
- Firewall is configured to only allow necessary ports
- Services run as non-root user (`psyop`)
- HTTPS should be configured for production use

## 📈 Scaling and Optimization

### Performance Tuning

- **Build caching**: Stack builds are cached between deployments
- **Parallel builds**: Haskell builds can utilize multiple cores
- **Static file serving**: Nginx serves static files directly

### Monitoring

- **Health checks**: Built-in health check endpoints
- **Logging**: Comprehensive logging for debugging
- **Metrics**: Can be extended with monitoring tools

## 🆘 Support

If you encounter issues:

1. Check the logs first
2. Verify all services are running
3. Ensure firewall rules are correct
4. Check GitHub webhook configuration
5. Verify file permissions and ownership

## 🔄 Manual Deployment

If you need to deploy manually:

```bash
# Deploy current code
./auto-deploy.sh deploy

# Or pull and restart manually
git pull origin main
stack build
sudo systemctl restart psyop-website
```

## 📝 Environment Variables

Key environment variables (set in `/etc/environment`):

- `WEBHOOK_SECRET`: Secret for webhook verification
- `WEBHOOK_PORT`: Port for webhook server (default: 9000)
- `REPO_PATH`: Path to application directory
- `PORT`: Application port (default: 8080)

---

**Result**: With this setup, every time you `git push` to the main branch, your website will automatically update within 30-60 seconds, providing true real-time deployment.
