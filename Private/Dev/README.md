# PSYOP Development Scripts

This directory contains essential scripts for the main branch deployment and server configuration.

## 📋 **Available Scripts**

| Script | Purpose | Usage |
|--------|---------|-------|
| `deploy.sh` | Enhanced deployment helper with multiple modes | `./deploy.sh [options]` |
| `deploy-auto.sh` | Automated deployment pipeline | `./deploy-auto.sh` |
| `smoke-tests.sh` | Basic website functionality tests | `./smoke-tests.sh` |
| `setup-server.sh` | Complete server configuration | `./setup-server.sh` |

## 🚀 **Quick Start**

### **Deploy Website**
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

## 📚 **Documentation**

- **Main README**: `/README.md` - Project overview and setup
- **Deployment Guide**: See `deploy.sh --help` for options
- **Server Setup**: `setup-server.sh` installs all development tools

## 🔧 **What's Included**

### **Deployment Scripts**
- `deploy.sh`: Multi-mode deployment (manual, auto, realtime)
- `deploy-auto.sh`: Full automation pipeline
- `smoke-tests.sh`: Basic functionality verification

### **Server Configuration**
- `setup-server.sh`: Complete development environment setup
  - Node.js, Rust, Go, Python tools
  - Docker, Neovim, Oh My Zsh
  - Tmux, Haskell Stack, development tools
  - Firewall and security configuration

## 🚫 **Removed Scripts**

The following scripts were removed as they're not needed on the main branch:
- `hot-reload.sh` - Development server (for dev branches)
- `monitor-git.sh` - Git monitoring (for dev branches)
- `webhook-deploy.sh` - Webhook server (for dev branches)
- `setup-realtime.sh` - Real-time setup (for dev branches)
- Real-time deployment services and documentation

These scripts are available on development branches where real-time deployment is needed.

---

**Main branch focuses on production deployment and server configuration.**
