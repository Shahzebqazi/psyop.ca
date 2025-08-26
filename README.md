### LLM Review Instructions
```yaml
llm_review:
  goal: "Take over development on the fallback branch via SSH; read private/docs and continue tests/deploy"
  scope:
    - path: "private/docs/TRD.yaml"
    - path: "private/docs/fallback-server-implementation.yaml"
    - path: "private/docs/WORKLOG.log"
    - path: "private/docs/CHANGELOG.log"
    - path: "private/docs/environment.md"
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
      - "Read docs under private/docs before changes"
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
See `private/docs/overview.md` for detailed documentation moved from this README.
See `private/docs/environment.md` for environment variable guidance.

## 🔧 Environment files

- `env.example`: Example values and structure for local development.
- `env.template`: Blank template to copy to `.env` and fill in.

Full details: `private/docs/environment.md`.

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
./private/dev/hot-reload.sh

# Start server manually
stack run

# Clean build artifacts
stack clean
```

## 📦 Changelog

See `private/docs/CHANGELOG.log` for the full, timestamped changelog.

## 🧰 Worklog

See `private/docs/WORKLOG.log` for the full, timestamped worklog.

## 📄 License

This project is licensed under the BSD-3-Clause License - see the [LICENSE](LICENSE) file for details.

## 🆘 Support

For support and questions:
- **Email**: admin@psyop.ca
- **Issues**: [GitHub Issues](https://github.com/Shahzebqazi/psyop.ca/issues)
- **Documentation**: This README and inline code comments

## 🎵 About PSYOP

PSYOP is a metal band focused on creating powerful, atmospheric music. The website reflects the band's aesthetic with its dark, technical background system and modern web design.

---

**⚠️ WARNING: This branch is fallback-only and intentionally strips production front-end code. Do not use for production builds of the full site.**

**Built with ❤️ and Haskell**
