# PSYOP Real-Time Deployment System

This directory contains scripts for real-time deployment that automatically update your website when changes are pushed to the `origin/main` branch.

## 🚀 **Real-Time Deployment Options**

### **1. Git Monitor (Polling)**
- **Script**: `monitor-git.sh`
- **Service**: `psyop-git-monitor.service`
- **How it works**: Checks for remote changes every 30 seconds
- **Pros**: Simple, reliable, no external dependencies
- **Cons**: Slight delay (up to 30 seconds)

### **2. Webhook Server (Instant)**
- **Script**: `webhook-deploy.sh`
- **Service**: `psyop-webhook.service`
- **How it works**: Receives instant notifications from GitHub
- **Pros**: Instant deployment, real-time updates
- **Cons**: Requires GitHub webhook configuration

### **3. Automated Deployment**
- **Script**: `deploy-auto.sh`
- **How it works**: Manual trigger with full automation
- **Use case**: When you want to deploy immediately

## 🔧 **Quick Setup**

### **Option 1: One-Command Setup (Recommended)**
```bash
# Run from your production server
cd /opt/psyop/psyop.ca
./Private/Dev/setup-realtime.sh
```

### **Option 2: Manual Setup**
```bash
# 1. Install systemd services
sudo cp Private/Dev/psyop-git-monitor.service /etc/systemd/system/
sudo cp Private/Dev/psyop-webhook.service /etc/systemd/system/

# 2. Enable and start services
sudo systemctl enable psyop-git-monitor.service
sudo systemctl enable psyop-webhook.service
sudo systemctl start psyop-git-monitor.service
sudo systemctl start psyop-webhook.service
```

## 📋 **Script Overview**

| Script | Purpose | Usage |
|--------|---------|-------|
| `setup-realtime.sh` | Complete setup of real-time deployment | `./setup-realtime.sh` |
| `monitor-git.sh` | Git polling monitor | `./monitor-git.sh [options]` |
| `webhook-deploy.sh` | Webhook server for instant deployment | `./webhook-deploy.sh [options]` |
| `deploy-auto.sh` | Automated deployment pipeline | `./deploy-auto.sh` |
| `deploy.sh` | Enhanced deployment helper | `./deploy.sh -m realtime` |

## 🌐 **GitHub Webhook Configuration**

After running the setup, you'll get a `github-webhook-setup.md` file with detailed instructions.

### **Basic Webhook Setup:**
1. Go to your GitHub repository
2. Settings → Webhooks → Add webhook
3. **Payload URL**: `http://YOUR_SERVER_IP:9000/webhook`
4. **Content type**: `application/json`
5. **Secret**: Use the generated secret from setup
6. **Events**: Just the push event
7. **Branch**: main

## ⚙️ **Configuration Options**

### **Environment Variables**
```bash
# Git Monitor
CHECK_INTERVAL=30          # Check every 30 seconds
BRANCH=main               # Branch to monitor
GIT_REMOTE=origin         # Git remote to monitor

# Webhook Server
WEBHOOK_PORT=9000         # Webhook server port
WEBHOOK_SECRET=secret     # Webhook validation secret
BRANCH=main               # Branch to monitor
```

### **Command Line Options**
```bash
# Git Monitor
./monitor-git.sh -i 60    # Check every 60 seconds
./monitor-git.sh -b develop # Monitor develop branch

# Webhook Server
./webhook-deploy.sh -p 8080    # Use port 8080
./webhook-deploy.sh -s secret  # Set webhook secret
```

## 🔄 **How Real-Time Deployment Works**

### **Git Monitor Flow:**
1. **Polling**: Checks `origin/main` every 30 seconds
2. **Detection**: Compares local vs remote commit hashes
3. **Update**: Pulls latest changes if different
4. **Build**: Compiles the project with Stack
5. **Restart**: Restarts the systemd service
6. **Verify**: Runs smoke tests to confirm deployment

### **Webhook Flow:**
1. **Trigger**: GitHub sends webhook on push to main
2. **Validation**: Webhook server validates signature
3. **Branch Check**: Confirms it's for main branch
4. **Instant Update**: Immediately pulls and deploys
5. **Verification**: Runs tests and confirms success

## 📊 **Monitoring and Logs**

### **Service Status**
```bash
# Check service status
systemctl status psyop-git-monitor.service
systemctl status psyop-webhook.service

# View logs
journalctl -u psyop-git-monitor.service -f
journalctl -u psyop-webhook.service -f
```

### **Log Files**
- **Git Monitor**: `/var/log/psyop/git-monitor.log`
- **Webhook Server**: `/var/log/psyop/webhook.log`
- **Main Service**: `/var/log/psyop/server.log`

## 🛠️ **Troubleshooting**

### **Common Issues**

1. **Git Monitor Not Working**
   ```bash
   # Check if service is running
   systemctl status psyop-git-monitor.service
   
   # Check logs
   journalctl -u psyop-git-monitor.service -f
   
   # Verify git access
   cd /opt/psyop/psyop.ca
   git fetch origin main
   ```

2. **Webhook Not Triggering**
   ```bash
   # Check webhook service
   systemctl status psyop-webhook.service
   
   # Verify port is open
   netstat -tlnp | grep 9000
   
   # Test webhook manually
   curl -X POST http://localhost:9000/webhook
   ```

3. **Build Failures**
   ```bash
   # Check Stack installation
   stack --version
   
   # Verify dependencies
   stack build --dry-run
   
   # Check system resources
   df -h
   free -h
   ```

### **Debug Mode**
```bash
# Run with debug output
bash -x Private/Dev/monitor-git.sh
bash -x Private/Dev/webhook-deploy.sh

# Check systemd service logs
journalctl -u psyop-git-monitor.service -f --no-pager
```

## 🔒 **Security Considerations**

### **Webhook Security**
- **Secret Validation**: All webhooks are validated with HMAC-SHA256
- **Branch Filtering**: Only main branch pushes trigger deployment
- **Firewall**: Webhook port should be restricted to GitHub IPs
- **HTTPS**: Consider using HTTPS for webhook endpoints

### **Service Security**
- **User Isolation**: Services run as `psyop` user
- **File Permissions**: Restricted access to system files
- **Capabilities**: Minimal required system capabilities
- **Logging**: All actions are logged for audit

## 📈 **Performance and Scaling**

### **Optimization Tips**
- **Check Interval**: Adjust `CHECK_INTERVAL` based on your needs
- **Build Caching**: Stack caches build artifacts automatically
- **Parallel Builds**: Stack can build dependencies in parallel
- **Resource Limits**: Monitor system resources during builds

### **Scaling Considerations**
- **Multiple Instances**: Can run multiple git monitors for redundancy
- **Load Balancing**: Webhook server can handle multiple concurrent requests
- **Database**: Consider external database for deployment history
- **Monitoring**: Add external monitoring (Prometheus, Grafana)

## 🎯 **Best Practices**

1. **Always test locally** before pushing to main
2. **Use feature branches** for development
3. **Monitor deployment logs** regularly
4. **Set up alerts** for failed deployments
5. **Backup configuration** before major changes
6. **Document changes** in commit messages
7. **Review deployment history** periodically

## 🚀 **Getting Started**

### **Complete Setup (Recommended)**
```bash
# 1. Run setup script
./Private/Dev/setup-realtime.sh

# 2. Configure GitHub webhook using generated instructions

# 3. Test with a small change
echo "# Test" >> README.md
git add README.md
git commit -m "Test real-time deployment"
git push origin main

# 4. Watch your website update automatically!
```

### **Manual Testing**
```bash
# Test git monitor
./Private/Dev/monitor-git.sh -i 10

# Test webhook server
./Private/Dev/webhook-deploy.sh -p 9001

# Test automated deployment
REMOTE_HOST=your-server.com ./Private/Dev/deploy-auto.sh
```

## 📞 **Support**

For issues or questions:
- Check the logs first: `journalctl -u service-name -f`
- Review this documentation
- Check the main README.md
- Contact: admin@psyop.ca

---

**🎉 Your website will now update automatically when you push to main!**
