# PSYOP Website Documentation Overview

This document consolidates technical documentation for the PSYOP website. It provides an overview of the system architecture, server configuration, styling architecture, background system, mobile features, performance considerations, and future enhancements.

## Project Overview

PSYOP is a modern, responsive website for the metal band PSYOP, built in Haskell with a component-based architecture. **This is now the live production branch serving psyop.ca.**

## Architecture

### Technology Stack
- Backend: Haskell with WAI/Warp
- Frontend: Blaze HTML with CSS3 and JavaScript
- Styling: Responsive CSS with Flexbox and Grid
- Background: WebGL-powered dynamic backgrounds and fallback ASCII art system
- Production: HTTPS-enabled with systemd service management

### Project Structure
```
psyop.ca/
├── Private/
│   ├── Src/
│   │   ├── Main.hs                 # Application entry point
│   │   ├── App.hs                  # Main application logic and routing
│   │   ├── Models.hs               # Data models and content generation
│   │   └── Main                    # Executable binary
│   ├── Assets/                     # Private assets and brand materials
│   │   ├── graphics/               # Promotional graphics and artwork
│   │   ├── photos/                 # Band photos and show images
│   │   └── webdev/                 # Web development assets and icons
│   ├── Content/                    # Content configuration
│   │   ├── Site.yaml               # Site configuration
│   │   └── psyop.json              # Multilingual definitions (40+ languages)
│   ├── Dev/                        # Development and deployment scripts
│   │   ├── deploy.sh               # Production deployment script
│   │   ├── hot-reload.sh           # Development hot-reload script
│   │   └── smoke-tests.sh          # Health check tests
│   ├── Docs/                       # Project documentation
│   │   ├── overview.md             # This overview document
│   │   ├── fallback-server-implementation.yaml # Fallback system specs
│   │   ├── WORKLOG.log             # Development work log
│   │   └── CHANGELOG.log           # Feature and change log
│   └── Tests/                      # Test specifications
├── Public/                         # Web-served static files
│   ├── css/                        # Stylesheets
│   ├── js/                         # JavaScript files
│   └── Assets/                     # Public assets
├── psyop-website.cabal             # Cabal project configuration
├── package.yaml                    # Hpack configuration
└── stack.yaml                      # Stack build configuration
```

## Server Configuration

- **Production Port**: 443 (HTTPS)
- **HTTP Redirect**: Port 80 → HTTPS
- **Framework**: WAI/Warp with TLS support
- **Service**: systemd-managed psyop-website.service
- **Routes**:
  - `/` - Main application with fallback system
  - `/lite` - No-CSS fallback version
  - `/css/style.css` - Main stylesheet
  - `/css/components.css` - Component styles
  - `/css/responsive.css` - Responsive design rules
  - `/health` - Health check endpoint

## CSS Architecture

### Main Styles (`Public/css/style.css`)
- Global styles and layout
- Section-specific styling
- WebGL background integration
- Responsive design foundation

### Component Styles (`Public/css/components.css`)
- MenuBar styling and animations
- Footer layout and social links
- Brand identity and typography
- Interactive elements

### Responsive Styles (`Public/css/responsive.css`)
- Mobile navigation
- Breakpoint-specific adjustments
- Touch-friendly interactions
- Mobile-optimized layouts

## Background System

### WebGL Background (Production)
- Hardware-accelerated rendering
- Dynamic shader effects
- Interactive mouse/touch support
- Smooth transitions and animations

### Fallback ASCII Art System
- Random content generation
- Language support (40+ languages)
- Special markers and content shuffling
- CSS-integrated background rendering

## Mobile Features

### Responsive Navigation
- Compact Header
- Side Hamburger Menu
- Overlay Menu System
- Touch Optimization

### Mobile-Specific Styling
- Font Scaling
- Adaptive Spacing
- Touch Animations
- Performance Optimization

## Production Features

### HTTPS and Security
- TLS 1.3 support
- Automatic HTTP to HTTPS redirects
- Security headers and CORS configuration
- Bot-aware fallback system

### Fallback System
- Graceful degradation for older browsers
- No-CSS lite version at `/lite`
- Content caching and optimization
- Health monitoring and logging

## Performance

### Current Metrics
- Build Time: ~10 seconds
- Server Start: ~3 seconds
- CSS Loading: <100ms
- Background Generation: <50ms

### Production Optimizations
- Asset compression and optimization
- CDN-ready static file serving
- Efficient routing and caching
- Systemd service management

## Development Workflow

### Local Development
```bash
# Start development server
./Private/Dev/hot-reload.sh

# Build project
stack build

# Run tests
stack test
```

### Production Deployment
```bash
# Deploy to production
./Private/Dev/deploy.sh

# Check service status
systemctl status psyop-website.service

# View logs
journalctl -u psyop-website.service -f
```

## Future Enhancements

### Content Management
- Dynamic content updates
- Admin panel interface
- Enhanced localization
- SEO optimization

### Performance
- Asset CDN integration
- Advanced caching strategies
- Performance monitoring
- A/B testing framework

## Contributing

- Follow Haskell best practices
- Maintain type safety and explicit signatures
- Use modular component architecture
- Add tests for new features
- Update documentation for changes

---

For high-level project usage, installation, and logs, see the repository `README.md`.


