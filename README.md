### LLM Review Instructions
```yaml
llm_review:
  goal: "Take over development on the fallback branch via SSH; read private/docs and continue tests/deploy"
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
    branch: "shahzebqazi/fallback"
    notes:
      - "Use Host header with 127.0.0.1 for local HTTPS curls"
      - "After stack build, run setcap on /opt/psyop/bin/psyop-website-exe"
      - "Read docs under Private/docs before changes"
  update_readme_logs:
    worklog:
      enabled: true
      location: "private/docs/WORKLOG.log"
      format: "- DD-MM-YYYY hh:mm:ss : entry (UTC)"
      order: "newest_first"
    changelog:
      enabled: true
      location: "private/docs/CHANGELOG.log"
      format: "### DD-MM-YYYY hh:mm:ss — summary"
      order: "newest_first"
  rules:
    - "Use UTC timestamps in DD-MM-YYYY hh:mm:ss format"
    - "Append-only: never delete past entries"
    - "Keep entries concise and actionable"
```

# Psyop - Website

> Note: Fallback branch only — not merging to `main`. This branch must contain only the minimal fallback website. All production front-end code (enhanced UI/components/assets not required for the fallback) should be removed or kept disabled. The canonical production site should not be built from this branch.

A modern, responsive website for the metal band PSYOP, built with Haskell featuring a component-based architecture and tomato red accent styling. **✅ All critical functionality has been restored and the website is now fully operational.**

## 📚 Documentation
See `Docs/overview.md` for detailed documentation moved from this README.
Environment variables are documented in the [Environment Variables](#-environment-variables) section below.

## 🔧 Environment Variables

The following environment variables can be configured for the Psyop website. Copy the ones you need to a `.env` file in the project root:

### Server Configuration
```bash
# Environment
ENVIRONMENT=development

# Server settings
PORT=8080
HTTPS_ENABLE=false
WWW_CANONICAL=true
REDIRECT_HTTPS=false
CERT_FILE=
KEY_FILE=
SERVER_HOST=localhost
```

### Security & Admin
```bash
# Admin configuration
ADMIN_EMAIL=admin@psyop.ca
ADMIN_USERNAME=admin

# Security
SESSION_SECRET=your_session_secret_here
CORS_ORIGINS=http://localhost:8080,https://psyop.ca
```

### Development & Logging
```bash
# Development
HOT_RELOAD_ENABLED=true
DEBUG_MODE=true

# Logging
LOG_LEVEL=debug
LOG_FILE=./logs/psyop.log
```

### Asset Configuration
```bash
# Assets
ASSET_CDN_URL=
ASSET_LOCAL_PATH=./assets
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
./Private/dev/hot-reload.sh

# Start server manually
stack run

# Clean build artifacts
stack clean
```

## 📦 Changelog

See `Docs/CHANGELOG.log` for the full, timestamped changelog.

## 🧰 Worklog

See `Docs/WORKLOG.log` for the full, timestamped worklog.

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

**⚠️ WARNING: This branch is fallback-only and intentionally strips production front-end code. Do not use for production builds of the full site.**

**Built with ❤️ and Haskell**
