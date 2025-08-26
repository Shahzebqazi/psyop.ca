// ===== PSYOP Scroll Enhancement =====
// Smooth scrolling and section highlighting

document.addEventListener('DOMContentLoaded', () => {
    console.log('Scroll enhancement script loaded');
    
    const sections = document.querySelectorAll('.page');
    const navLinks = document.querySelectorAll('.nav-link');
    const sectionNavLinks = document.querySelectorAll('.section-nav-link');
    
    console.log('Found sections:', sections.length);
    console.log('Found nav links:', navLinks.length);
    console.log('Found section nav links:', sectionNavLinks.length);
    
    // Smooth scroll to section when nav links are clicked
    navLinks.forEach(link => {
        link.addEventListener('click', (e) => {
            e.preventDefault();
            const targetId = link.getAttribute('href').substring(1);
            const targetSection = document.getElementById(targetId);
            
            console.log('Nav link clicked:', targetId, targetSection);
            
            if (targetSection) {
                targetSection.scrollIntoView({ behavior: 'smooth' });
            }
        });
    });
    
    // Smooth scroll to section when section nav links are clicked
    sectionNavLinks.forEach(link => {
        link.addEventListener('click', (e) => {
            e.preventDefault();
            const targetId = link.getAttribute('href').substring(1);
            const targetSection = document.getElementById(targetId);
            
            console.log('Section nav link clicked:', targetId, targetSection);
            
            if (targetSection) {
                targetSection.scrollIntoView({ behavior: 'smooth' });
            }
        });
    });
    
    // Update active nav link based on current section
    const updateActiveNav = () => {
        const scrollPosition = window.scrollY + 100;
        
        sections.forEach(section => {
            const sectionTop = section.offsetTop;
            const sectionHeight = section.offsetHeight;
            const sectionId = section.getAttribute('id');
            
            if (scrollPosition >= sectionTop && scrollPosition < sectionTop + sectionHeight) {
                // Remove active class from all nav links
                navLinks.forEach(link => link.classList.remove('active'));
                sectionNavLinks.forEach(link => link.classList.remove('active'));
                
                // Add active class to current section's nav link
                const activeLink = document.querySelector(`.nav-link[href="#${sectionId}"]`);
                if (activeLink) {
                    activeLink.classList.add('active');
                }
                
                // Add active class to current section's section nav link
                const activeSectionLink = document.querySelector(`.section-nav-link[href="#${sectionId}"]`);
                if (activeSectionLink) {
                    activeSectionLink.classList.add('active');
                }
                
                // Add active class to current section
                sections.forEach(s => s.classList.remove('active'));
                section.classList.add('active');
            }
        });
    };
    
    // Throttled scroll event for better performance
    let ticking = false;
    const handleScroll = () => {
        if (!ticking) {
            requestAnimationFrame(() => {
                updateActiveNav();
                ticking = false;
            });
            ticking = true;
        }
    };
    
    // Add scroll event listener
    window.addEventListener('scroll', handleScroll);
    
    // Initial call to set active section
    updateActiveNav();
    
    // Add scroll progress indicator
    const createScrollProgress = () => {
        const progressBar = document.createElement('div');
        progressBar.className = 'scroll-progress';
        progressBar.style.cssText = `
            position: fixed;
            top: 0;
            left: 0;
            width: 0%;
            height: 3px;
            background: linear-gradient(45deg, #ff6b6b, #4ecdc4);
            z-index: 1000;
            transition: width 0.1s ease;
        `;
        document.body.appendChild(progressBar);
        
        // Update progress bar on scroll
        const updateProgress = () => {
            const scrollTop = window.scrollY;
            const docHeight = document.documentElement.scrollHeight - window.innerHeight;
            const scrollPercent = (scrollTop / docHeight) * 100;
            progressBar.style.width = scrollPercent + '%';
        };
        
        window.addEventListener('scroll', updateProgress);
        updateProgress(); // Initial call
    };
    
    createScrollProgress();
    
    // Add test scroll function to global scope
    window.testScroll = () => {
        console.log('Test scroll function called');
        const musicSection = document.getElementById('music');
        if (musicSection) {
            console.log('Scrolling to music section');
            musicSection.scrollIntoView({ behavior: 'smooth' });
        } else {
            console.log('Music section not found');
        }
    };
});
