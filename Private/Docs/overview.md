# PSYOP Website Documentation Overview

This document consolidates technical documentation that was previously in the project `README.md`. It provides an overview of the system architecture, server configuration, styling architecture, background system, mobile features, performance considerations, and future enhancements.

## Project Overview

PSYOP is a modern, responsive website for the metal band PSYOP, built in Haskell with a component-based architecture.

## Architecture

### Technology Stack
- Backend: Haskell with WAI/Warp
- Frontend: Blaze HTML with CSS3 and JavaScript
- Styling: Responsive CSS with Flexbox and Grid
- Background: ASCII art system with randomization (currently broken)

### Project Structure
```
psyop.ca/
├── src/
│   ├── Main.hs                 # Application entry point
│   ├── App.hs                  # Main application logic and routing
│   ├── Models.hs               # Data models and ASCII wallpaper generation
│   ├── Views.hs                # Pure HTML presentation components
│   ├── Lib.hs                  # Core library functions
│   └── components/             # UI Component modules
│       ├── MenuBar.hs          # Navigation component with tomato red styling
│       └── Footer.hs           # Footer component with psychological warfare theme
├── private/                    # Private development and documentation
│   ├── dev/
│   │   └── hot-reload.sh       # Consolidated development hot-reload script
│   └── docs/                   # Project documentation
├── assets/                     # Static assets and brand materials
├── public/                     # Web-served static files
├── psyop.txt                   # Multilingual definitions (33 languages)
├── psyop-website.cabal         # Cabal project configuration
└── package.yaml                # Hpack configuration
```

## Server Configuration

- Port: 8080
- Framework: WAI/Warp
- Routes:
  - `/` - Main application
  - `/css/style.css` - Main stylesheet
  - `/css/components.css` - Component styles
  - `/css/responsive.css` - Responsive design rules

## CSS Architecture

### Main Styles (`style.css`)
- Global styles and layout
- Section-specific styling
- ASCII art background integration (BROKEN - no special styling)
- WebGL container preparation

### Component Styles (`components.css`)
- MenuBar styling and animations (BROKEN - subtitle animation not working)
- Footer layout and social links
- Brand identity and typography
- Interactive elements

### Responsive Styles (`responsive.css`)
- Mobile navigation (BROKEN - menu toggle not working)
- Breakpoint-specific adjustments
- Touch-friendly interactions
- Mobile-optimized layouts

## Background System (BROKEN)

### ASCII Art Generation
The background system generates dynamic content using:
- Random Seeds: Different content for each section
- Content Shuffling: Randomized language ordering
- Special Markers: ACCESS DENIED and machine code elements (not styled)
- CSS Integration: Seamless background rendering (limited by CSS constraints)

### Language Support
Currently supports 33 languages including English, Spanish, French, German, Japanese, Korean, Chinese, Arabic, Persian, Turkish, and more.

## Mobile Features (PARTIALLY BROKEN)

### Responsive Navigation
- Compact Header
- Side Hamburger (BROKEN - doesn't open menu)
- Overlay Menu (BROKEN - not accessible)
- Touch Optimization

### Mobile-Specific Styling
- Font Scaling
- Spacing
- Animations
- Performance

## Performance

### Current Metrics
- Build Time: ~10 seconds
- Server Start: ~3 seconds
- CSS Loading: <100ms
- Background Generation: <50ms

### Optimization Goals
- Lazy Loading: Background generation on demand
- Caching: CSS and content caching
- Compression: Gzip compression for assets
- CDN: Content delivery network integration

## Future Enhancements

### WebGL Background System
- Shader-Based GLSL effects
- Hardware-accelerated rendering
- Interactivity (mouse/touch)
- Smooth transitions

### Content Management
- Dynamic content
- Admin panel
- Localization
- SEO optimization

## Contributing

- Haskell best practices
- Type safety and explicit signatures
- Modular component architecture
- Add tests for new features

---

For high-level project usage, installation, and logs, see the repository `README.md`.


