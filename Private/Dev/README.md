# PSYOP Development Scripts

This directory contains essential scripts for the main branch deployment and server configuration, **including automatic real-time deployment when you push to main**.

## 📋 **Available Scripts**

| Script | Purpose | Usage |
|--------|---------|-------|
| `deploy.sh` | Enhanced deployment helper with multiple modes | `./deploy.sh [options]` |
| `deploy-auto.sh` | Automated deployment pipeline | `./deploy-auto.sh` |
| `monitor-git.sh` | **Git monitoring for automatic deployment** | `./monitor-git.sh [options]` |
| `setup-realtime.sh` | **Set up automatic real-time deployment** | `./setup-realtime.sh` |
| `smoke-tests.sh` | Basic website functionality tests | `./smoke-tests.sh` |
| `setup-server.sh` | Complete server configuration | `./setup-server.sh` |

## 🚀 **Quick Start**

### **Set Up Automatic Deployment (Recommended)**
```bash
# Run on your production server to enable auto-deployment
cd /opt/psyop/psyop.ca/Private/Dev
./setup-realtime.sh
```

### **Deploy Website Manually**
```bash
# Manual deployment
./deploy.sh -h your-server.com

# Automated deployment
REMOTE_HOST=your-server.com ./deploy-auto.sh
```

### **Configure Server**
```bash
# Run as regular user (not root)
./setup-server.sh
```

### **Run Tests**
```bash
# Test against localhost
./smoke-tests.sh

# Test against remote server
HOST=https://your-server.com ./smoke-tests.sh
```

## 🔄 **Real-Time Deployment (NEW!)**

**Your website now updates automatically when you push to main!**

### **How It Works:**
1. **Git Monitor Service** runs continuously on your server
2. **Checks every 30 seconds** for changes on `origin/main`
3. **Automatically deploys** when new commits are detected:
   - Pulls latest code
   - Builds the project
   - Restarts the website service
   - Runs smoke tests

### **Setup:**
```bash
# One command to enable automatic deployment
./setup-realtime.sh
```

### **Monitor:**
```bash
# Check service status
systemctl status psyop-git-monitor.service

# View logs
journalctl -u psyop-git-monitor.service -f
```

## 📚 **Documentation**

- **Main README**: `/README.md` - Project overview and setup
- **Scripts README**: `Private/Dev/README.md` - Script documentation
- **Server Setup**: `setup-server.sh` - Complete server configuration
- **Real-Time Setup**: `setup-realtime.sh` - Automatic deployment setup

## 🔧 **What's Included**

### **Deployment Scripts**
- `deploy.sh`: Multi-mode deployment (manual, auto, realtime)
- `deploy-auto.sh`: Full automation pipeline
- `monitor-git.sh`: **Git monitoring for automatic deployment**
- `setup-realtime.sh`: **Real-time deployment setup**
- `smoke-tests.sh`: Basic functionality verification

### **Server Configuration**
- `setup-server.sh`: Complete development environment setup
  - Node.js, Rust, Go, Python tools
  - Docker, Neovim, Oh My Zsh
  - Tmux, Haskell Stack, development tools
  - Firewall and security configuration

## 🎯 **Workflow**

### **With Real-Time Deployment (Recommended):**
1. **Set up once**: `./setup-realtime.sh`
2. **Develop locally**: Make changes to your code
3. **Push to main**: `git push origin main`
4. **Automatic deployment**: Website updates within 30 seconds!

### **Manual Deployment:**
1. **Make changes** to your code
2. **Push to main**: `git push origin main`
3. **Run deployment**: `./deploy-auto.sh` or `./deploy.sh`

## 🚫 **Removed Scripts**

The following scripts were removed as they're not needed on the main branch:
- `hot-reload.sh` - Development server (for dev branches)
- `webhook-deploy.sh` - Webhook server (for dev branches)
- Real-time deployment services and documentation

These scripts are available on development branches where real-time deployment is needed.

---

**🎉 Main branch now has automatic real-time deployment when you push to main!**
