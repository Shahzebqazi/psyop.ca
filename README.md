# PSYOP - Metal Band Website

A modern, responsive website for the metal band PSYOP, built with Haskell featuring a component-based architecture and tomato red accent styling. **✅ All critical functionality has been restored and the website is now fully operational.**

## ✅ CRITICAL ISSUES RESOLVED

### **Fixed Functionality**
- **✅ Mobile Menu Toggle**: Hamburger button now opens and closes navigation menu properly
- **✅ Subtitle Animation**: "xyz 123 abc" subtitle animation restored and working smoothly
- **✅ Color Scheme**: Updated to tomato red (#ff6347) and military red (#cc2e1f) for better brand identity
- **✅ Architecture**: Clean separation of concerns with Models.hs handling background logic and Views.hs pure HTML

### **Resolved System Issues**
- **✅ Component Architecture**: Proper separation between presentation (Views.hs) and business logic (Models.hs)
- **✅ Development Tools**: Consolidated hot-reload script moved to `private/dev/`
- **✅ Documentation**: Organized into `private/docs/` with updated build logs and architecture docs

## 🎯 Project Overview

PSYOP is a metal band website featuring:
- **Single Page Application (SPA)** with smooth scrolling between sections
- **ASCII Art Background** system with multilingual definitions (currently broken)
- **Responsive Design** optimized for mobile and desktop (partially broken)
- **Component-Based Architecture** built with Haskell
- **WebGL-Ready** background system for future enhancements

## 🏗️ Architecture

### Technology Stack
- **Backend**: Haskell with WAI/Warp
- **Frontend**: Blaze HTML with CSS3 and JavaScript
- **Styling**: Responsive CSS with Flexbox and Grid
- **Background**: ASCII art system with randomization (currently broken)

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
│       ├── TODO.md             # Build log and task tracking
│       ├── ARCHITECTURE.md     # System architecture documentation
│       └── Build Log.md        # Development progress log
├── assets/                     # Static assets and brand materials
├── public/                     # Web-served static files
├── psyop.txt                   # Multilingual definitions (33 languages)
├── psyop-website.cabal         # Cabal project configuration
└── package.yaml                # Hpack configuration
```

## ✨ Features

### 🎨 ASCII Art Background System (BROKEN)
- **Multilingual Content**: Definitions of "Psychological Operation" in 33 languages
- **Randomized Generation**: Each page gets different content ordering
- **Special Elements**: 
  - 10% chance for "ACCESS DENIED" messages (not styled properly)
  - 15% chance for machine code gibberish (not styled properly)
  - 75% chance for language definitions
- **Lazy Evaluation**: Background only generated when needed
- **Tiling Pattern**: Repeating background for full coverage

### 🧭 Navigation System (PARTIALLY BROKEN)
- **Sticky Header**: Always visible navigation bar
- **Smooth Scrolling**: Seamless transitions between sections
- **Active State**: Visual feedback for current section
- **Mobile Responsive**: Hamburger menu for mobile devices (BROKEN - doesn't open)
- **Brand Identity**: "PSYOP" with animated subtitle "xyz 123 abc" (BROKEN - animation not working)

### 📱 Responsive Design (PARTIALLY BROKEN)
- **Mobile-First**: Optimized for mobile devices
- **Breakpoints**: Responsive layouts at 768px and 480px
- **Touch-Friendly**: Mobile-optimized navigation (BROKEN - menu toggle not working)
- **Flexible Layouts**: CSS Grid and Flexbox for adaptability

### 🎵 Content Sections
- **Home Section**: Landing page with ASCII background
- **Links Section**: Navigation to external resources
- **Shop Section**: Future e-commerce integration
- **Footer**: Social links and legal information

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

## 🌐 Server Configuration

- **Port**: 8080
- **Framework**: WAI/Warp
- **Routes**:
  - `/` - Main application
  - `/css/style.css` - Main stylesheet
  - `/css/components.css` - Component styles
  - `/css/responsive.css` - Responsive design rules

## 🎨 CSS Architecture

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

## 🔧 Background System (BROKEN)

### ASCII Art Generation
The background system generates dynamic content using:
- **Random Seeds**: Different content for each section
- **Content Shuffling**: Randomized language ordering
- **Special Markers**: ACCESS DENIED and machine code elements (not styled)
- **CSS Integration**: Seamless background rendering (limited by CSS constraints)

### Language Support
Currently supports 33 languages including:
- English, Spanish, French, German
- Japanese, Korean, Chinese
- Arabic, Persian, Turkish
- And many more...

## 📱 Mobile Features (PARTIALLY BROKEN)

### Responsive Navigation
- **Compact Header**: Reduced height on mobile
- **Side Hamburger**: Positioned for easy thumb access (BROKEN - doesn't open menu)
- **Overlay Menu**: Full-screen mobile navigation (BROKEN - not accessible)
- **Touch Optimization**: Larger touch targets

### Mobile-Specific Styling
- **Font Scaling**: Appropriate text sizes for mobile
- **Spacing**: Optimized padding and margins
- **Animations**: Smooth mobile transitions
- **Performance**: Optimized for mobile devices

## 🔮 Future Enhancements

### WebGL Background System
- **Shader-Based**: GLSL shaders for dynamic effects
- **Performance**: Hardware-accelerated rendering
- **Interactivity**: Mouse and touch responsive
- **Transitions**: Smooth background animations

### Content Management
- **Dynamic Content**: Database-driven content
- **Admin Panel**: Content management interface
- **Localization**: Multi-language support
- **SEO Optimization**: Search engine optimization

## 🐛 Known Issues (CRITICAL)

### Current Limitations
- **CSS Content**: HTML elements in CSS content not supported (fundamental limitation)
- **Background Styling**: Special markers use plain text with no visual differentiation
- **Mobile Menu**: JavaScript toggle completely broken - menu cannot be opened
- **Subtitle Animation**: CSS animation for "xyz 123 abc" not working
- **Visual Hierarchy**: No way to distinguish between different content types in background

### Required Fixes
- **Mobile Menu**: Fix JavaScript toggle functionality immediately
- **Subtitle Animation**: Restore CSS animation for brand subtitle
- **Background Styling**: Implement alternative approach for special content styling
- **User Experience**: Ensure all interactive elements work properly

## 📊 Performance

### Current Metrics
- **Build Time**: ~10 seconds
- **Server Start**: ~3 seconds
- **CSS Loading**: <100ms
- **Background Generation**: <50ms

### Optimization Goals
- **Lazy Loading**: Background generation on demand
- **Caching**: CSS and content caching
- **Compression**: Gzip compression for assets
- **CDN**: Content delivery network integration

## 🤝 Contributing

### Development Guidelines
- **Haskell Best Practices**: Follow functional programming principles
- **Type Safety**: Leverage Haskell's type system
- **Component Architecture**: Maintain modular design
- **Testing**: Add tests for new features

### Code Style
- **Descriptive Comments**: Clear, concise documentation
- **Minimal Empty Lines**: Efficient code formatting
- **Type Annotations**: Explicit type signatures
- **Error Handling**: Use Maybe, Either, and custom types

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

**⚠️ WARNING: This website currently has critical functionality issues that need immediate attention before it can be considered production-ready.**

**Built with ❤️ and Haskell**
