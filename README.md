### LLM Review Instructions
```yaml
llm_review:
  goal: "Maintain and develop the live production website branch"
  scope:
    - path: "Private/docs/TRD.yaml"
    - path: "Private/docs/fallback-server-implementation.yaml"
    - path: "Private/docs/WORKLOG.log"
    - path: "Private/docs/CHANGELOG.log"
    - path: "Private/docs/environment.md"
  checks:
    - "Spec completeness and contradictions"
    - "Routes and endpoints match implementation"
    - "Security: TLS, redirects, robots, sitemap"
    - "Terminology and file paths align with repo"
  context:
    ssh: true
    server: "psyop.ca host (Ubuntu, systemd)"
    cwd: "/opt/psyop/psyop.ca"
    branch: "main (live deployment)"
    notes:
      - "Use Host header with 127.0.0.1 for local HTTPS curls"
      - "After stack build, run setcap on /opt/psyop/bin/psyop-website-exe"
      - "Read docs under Private/docs before changes"
  update_readme_logs:
    worklog:
      enabled: true
      location: "Private/docs/WORKLOG.log"
      format: "- DD-MM-YYYY hh:mm:ss : entry (UTC)"
      order: "newest_first"
    changelog:
      enabled: true
      location: "Private/docs/CHANGELOG.log"
      format: "### DD-MM-YYYY hh:mm:ss — summary"
      order: "newest_first"
  rules:
    - "Use UTC timestamps in DD-MM-YYYY hh:mm:ss format"
    - "Append-only: never delete past entries"
    - "Keep entries concise and actionable"
```

# Psyop - Website

> **🚀 LIVE DEPLOYMENT BRANCH** — This is now the main production branch that serves psyop.ca live. All production functionality, enhanced UI components, and assets are included and actively maintained.

A modern, responsive website for the metal band PSYOP, built with Haskell featuring a component-based architecture, WebGL-powered dynamic backgrounds, and comprehensive fallback systems. **✅ Fully operational and serving live at psyop.ca**

## 📚 Documentation
See `Private/Docs/overview.md` for detailed documentation. Environment variables are documented in the [Environment Variables](#-environment-variables) section below.

## 🔧 Environment Variables

The following environment variables can be configured for the Psyop website. Copy the ones you need to a `.env` file in the project root:

### Server Configuration
```bash
# Environment
ENVIRONMENT=production

# Server settings
PORT=443
HTTPS_ENABLE=true
WWW_CANONICAL=true
REDIRECT_HTTPS=true
CERT_FILE=/etc/ssl/certs/psyop.crt
KEY_FILE=/etc/ssl/private/psyop.key
SERVER_HOST=psyop.ca
```

### Security & Admin
```bash
# Admin configuration
ADMIN_EMAIL=admin@psyop.ca
ADMIN_USERNAME=admin

# Security
SESSION_SECRET=your_session_secret_here
CORS_ORIGINS=https://psyop.ca,https://www.psyop.ca
```

### Development & Logging
```bash
# Development
HOT_RELOAD_ENABLED=false
DEBUG_MODE=false

# Logging
LOG_LEVEL=info
LOG_FILE=/var/log/psyop/server.log
```

### Asset Configuration
```bash
# Assets
ASSET_CDN_URL=
ASSET_LOCAL_PATH=./Private/Assets
```

### Future Extensions (commented out)
```bash
# Database (if needed in future)
# DATABASE_URL=postgresql://user:password@localhost:5432/psyop_db

# External API Keys (if needed in future)
# SPOTIFY_CLIENT_ID=your_spotify_client_id
# SPOTIFY_CLIENT_SECRET=your_spotify_client_secret
# APPLE_MUSIC_KEY=your_apple_music_key
```

**Note**: Create a `.env` file in the project root and add the variables you need. Never commit `.env` files to version control.

## 🚀 Getting Started

### Prerequisites
- Haskell Stack (GHC 9.6.6+)
- Git

### Installation
```bash
# Clone the repository
git clone https://github.com/Shahzebqazi/psyop.ca.git
cd psyop.ca

# Install dependencies
stack build

# Start the development server
stack run
```

### Development
```bash
# Build the project
stack build

# Run with hot-reload development server
./Private/Dev/hot-reload.sh

# Start server manually
stack run

# Clean build artifacts
stack clean
```

### Production Deployment
```bash
# Deploy to production server
./Private/Dev/deploy.sh

# Check service status
systemctl status psyop-website.service

# View logs
journalctl -u psyop-website.service -f
```

## 📦 Changelog

See `Private/Docs/CHANGELOG.log` for the full, timestamped changelog.

## 🧰 Worklog

See `Private/Docs/WORKLOG.log` for the full, timestamped worklog.

## 📄 License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details.

## 🆘 Support

For support and questions:
- **Email**: admin@psyop.ca
- **Issues**: [GitHub Issues](https://github.com/Shahzebqazi/psyop.ca/issues)
- **Documentation**: This README and inline code comments

## 🎵 About Psyop

Psyop is a metal band focused on creating powerful, atmospheric music. The website reflects the band's aesthetic with its dark, technical background system and modern web design.

---

**✅ This is the live production branch serving psyop.ca. All production features are included and actively maintained.**

**Built with ❤️ and Haskell**
