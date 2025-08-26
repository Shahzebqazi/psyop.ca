// PSYOP Website - Menu JavaScript
// Mobile menu functionality and navigation enhancements

class MenuManager {
    constructor() {
        this.mobileMenuToggle = document.getElementById('mobile-menu-toggle');
        this.mobileMenu = document.getElementById('mobile-menu');
        this.isMenuOpen = false;
        
        this.init();
    }
    
    init() {
        if (!this.mobileMenuToggle || !this.mobileMenu) {
            console.warn('Menu elements not found');
            return;
        }
        
        this.bindEvents();
        this.setupAccessibility();
    }
    
    bindEvents() {
        // Mobile menu toggle
        this.mobileMenuToggle.addEventListener('click', () => {
            this.toggleMenu();
        });
        
        // Close menu when clicking outside
        document.addEventListener('click', (e) => {
            if (!this.mobileMenu.contains(e.target) && 
                !this.mobileMenuToggle.contains(e.target)) {
                this.closeMenu();
            }
        });
        
        // Close menu on escape key
        document.addEventListener('keydown', (e) => {
            if (e.key === 'Escape') {
                this.closeMenu();
            }
        });
        
        // Handle window resize
        window.addEventListener('resize', () => {
            if (window.innerWidth > 768) {
                this.closeMenu();
            }
        });
        
        // Smooth scrolling for anchor links
        document.addEventListener('click', (e) => {
            if (e.target.matches('a[href^="#"]')) {
                e.preventDefault();
                const targetId = e.target.getAttribute('href').substring(1);
                const targetElement = document.getElementById(targetId);
                
                if (targetElement) {
                    this.scrollToElement(targetElement);
                    this.closeMenu();
                }
            }
        });
        
        // Add active state to current page in navigation
        this.highlightCurrentPage();
    }
    
    toggleMenu() {
        if (this.isMenuOpen) {
            this.closeMenu();
        } else {
            this.openMenu();
        }
    }
    
    openMenu() {
        this.isMenuOpen = true;
        this.mobileMenu.classList.add('active');
        this.mobileMenuToggle.classList.add('active');
        
        // Prevent body scroll
        document.body.style.overflow = 'hidden';
        
        // Focus first menu item for accessibility
        const firstMenuItem = this.mobileMenu.querySelector('.mobile-nav-link');
        if (firstMenuItem) {
            firstMenuItem.focus();
        }
        
        // Announce menu open to screen readers
        this.announceToScreenReader('Menu opened');
    }
    
    closeMenu() {
        this.isMenuOpen = false;
        this.mobileMenu.classList.remove('active');
        this.mobileMenuToggle.classList.remove('active');
        
        // Restore body scroll
        document.body.style.overflow = '';
        
        // Return focus to toggle button
        this.mobileMenuToggle.focus();
        
        // Announce menu closed to screen readers
        this.announceToScreenReader('Menu closed');
    }
    
    scrollToElement(element) {
        const headerHeight = 80; // Height of fixed header
        const elementPosition = element.offsetTop - headerHeight;
        
        window.scrollTo({
            top: elementPosition,
            behavior: 'smooth'
        });
    }
    
    highlightCurrentPage() {
        const currentPath = window.location.pathname;
        const navLinks = document.querySelectorAll('.nav-link, .mobile-nav-link');
        
        navLinks.forEach(link => {
            const linkPath = link.getAttribute('href');
            
            if (linkPath === currentPath || 
                (currentPath === '/' && linkPath === '/home') ||
                (currentPath === '/home' && linkPath === '/')) {
                link.classList.add('active');
            }
        });
    }
    
    setupAccessibility() {
        // Add ARIA labels and roles
        this.mobileMenuToggle.setAttribute('aria-label', 'Toggle mobile menu');
        this.mobileMenuToggle.setAttribute('aria-expanded', 'false');
        this.mobileMenuToggle.setAttribute('aria-controls', 'mobile-menu');
        
        this.mobileMenu.setAttribute('role', 'navigation');
        this.mobileMenu.setAttribute('aria-label', 'Mobile navigation');
        
        // Update ARIA attributes when menu state changes
        this.mobileMenuToggle.addEventListener('click', () => {
            const isExpanded = this.isMenuOpen;
            this.mobileMenuToggle.setAttribute('aria-expanded', isExpanded.toString());
        });
    }
    
    announceToScreenReader(message) {
        // Create a screen reader only announcement
        const announcement = document.createElement('div');
        announcement.setAttribute('aria-live', 'polite');
        announcement.setAttribute('aria-atomic', 'true');
        announcement.style.position = 'absolute';
        announcement.style.left = '-10000px';
        announcement.style.width = '1px';
        announcement.style.height = '1px';
        announcement.style.overflow = 'hidden';
        
        announcement.textContent = message;
        document.body.appendChild(announcement);
        
        // Remove after announcement
        setTimeout(() => {
            document.body.removeChild(announcement);
        }, 1000);
    }
}

// Navigation enhancements
class NavigationEnhancer {
    constructor() {
        this.header = document.querySelector('.header');
        this.lastScrollTop = 0;
        
        this.init();
    }
    
    init() {
        this.setupScrollEffects();
        this.setupSmoothScrolling();
        this.setupActiveLinkHighlighting();
    }
    
    setupScrollEffects() {
        let scrollTimeout;
        
        window.addEventListener('scroll', () => {
            const scrollTop = window.pageYOffset || document.documentElement.scrollTop;
            
            // Add/remove scrolled class for header styling
            if (scrollTop > 50) {
                this.header.classList.add('scrolled');
            } else {
                this.header.classList.remove('scrolled');
            }
            
            // Hide/show header on scroll (optional)
            if (scrollTop > this.lastScrollTop && scrollTop > 100) {
                // Scrolling down
                this.header.classList.add('header-hidden');
            } else {
                // Scrolling up
                this.header.classList.remove('header-hidden');
            }
            
            this.lastScrollTop = scrollTop;
            
            // Clear timeout for smooth transitions
            clearTimeout(scrollTimeout);
            scrollTimeout = setTimeout(() => {
                this.header.classList.remove('header-hidden');
            }, 1000);
        });
    }
    
    setupSmoothScrolling() {
        // Smooth scroll for internal links
        const internalLinks = document.querySelectorAll('a[href^="#"]');
        
        internalLinks.forEach(link => {
            link.addEventListener('click', (e) => {
                const href = link.getAttribute('href');
                if (href === '#') return;
                
                const targetElement = document.querySelector(href);
                if (targetElement) {
                    e.preventDefault();
                    this.scrollToElement(targetElement);
                }
            });
        });
    }
    
    scrollToElement(element) {
        const headerHeight = 80;
        const elementPosition = element.offsetTop - headerHeight;
        
        window.scrollTo({
            top: elementPosition,
            behavior: 'smooth'
        });
    }
    
    setupActiveLinkHighlighting() {
        // Highlight current section based on scroll position
        const sections = document.querySelectorAll('section[id], .page');
        const navLinks = document.querySelectorAll('.nav-link');
        
        window.addEventListener('scroll', () => {
            const scrollPosition = window.scrollY + 100;
            
            sections.forEach(section => {
                const sectionTop = section.offsetTop;
                const sectionHeight = section.offsetHeight;
                const sectionId = section.getAttribute('id');
                
                if (scrollPosition >= sectionTop && scrollPosition < sectionTop + sectionHeight) {
                    // Remove active class from all links
                    navLinks.forEach(link => link.classList.remove('active'));
                    
                    // Add active class to corresponding link
                    const correspondingLink = document.querySelector(`.nav-link[href="#${sectionId}"]`);
                    if (correspondingLink) {
                        correspondingLink.classList.add('active');
                    }
                }
            });
        });
    }
}

// Initialize when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    // Initialize menu manager
    const menuManager = new MenuManager();
    
    // Initialize navigation enhancer
    const navigationEnhancer = new NavigationEnhancer();
    
    // Add loading animation
    document.body.classList.add('loaded');
    
    // Add scroll-based animations
    const observerOptions = {
        threshold: 0.1,
        rootMargin: '0px 0px -50px 0px'
    };
    
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('animate-in');
            }
        });
    }, observerOptions);
    
    // Observe elements for animation
    const animateElements = document.querySelectorAll('.platform-card, .social-card, .contact-section, .tour-item, .release-info');
    animateElements.forEach(el => observer.observe(el));
});

// Export classes for potential external use
window.MenuManager = MenuManager;
window.NavigationEnhancer = NavigationEnhancer;
