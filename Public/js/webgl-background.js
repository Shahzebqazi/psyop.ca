// ===== PSYOP WebGL Background System =====
// Real-time shader-based image rendering with smooth transitions

class WebGLBackground {
    constructor() {
        try {
            console.log('WebGLBackground constructor called');
            
            this.scene = null;
            this.camera = null;
            this.renderer = null;
            this.material = null;
            this.mesh = null;
            this.clock = new THREE.Clock();
            
            this.init();
            this.setupEventListeners();
            this.animate();
            
            console.log('WebGLBackground constructor completed');
        } catch (error) {
            console.error('Error in WebGLBackground constructor:', error);
            // Add fallback background
            document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            
            // Also create a fallback div
            try {
                const fallbackDiv = document.createElement('div');
                fallbackDiv.style.position = 'fixed';
                fallbackDiv.style.top = '0';
                fallbackDiv.style.left = '0';
                fallbackDiv.style.width = '100%';
                fallbackDiv.style.height = '100%';
                fallbackDiv.style.zIndex = '-1';
                fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
                document.body.insertBefore(fallbackDiv, document.body.firstChild);
                console.log('Constructor error fallback div created');
            } catch (fallbackError) {
                console.error('Failed to create constructor fallback div:', fallbackError);
            }
        }
    }
    
    init() {
        try {
            console.log('Initializing WebGL scene...');
            
            // Scene setup
            this.scene = new THREE.Scene();
            console.log('Scene created');
            
            // Camera setup
            this.camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
            this.camera.position.z = 2;
            console.log('Camera created at z=2');
            
            // Renderer setup
            this.renderer = new THREE.WebGLRenderer({ 
                alpha: true, 
                antialias: true
            });
            this.renderer.setSize(window.innerWidth, window.innerHeight);
            this.renderer.setClearColor(0x000000, 0);
            console.log('Renderer created');
            
            // Add to DOM
            document.body.insertBefore(this.renderer.domElement, document.body.firstChild);
            
            // Verify the canvas was added
            if (document.body.contains(this.renderer.domElement)) {
                console.log('Canvas successfully added to DOM');
            } else {
                console.error('Canvas was not added to DOM!');
            }
            this.renderer.domElement.style.position = 'fixed';
            this.renderer.domElement.style.top = '0';
            this.renderer.domElement.style.left = '0';
            this.renderer.domElement.style.width = '100%';
            this.renderer.domElement.style.height = '100%';
            this.renderer.domElement.style.zIndex = '-1';
            this.renderer.domElement.style.pointerEvents = 'none';
            
            // Add visual debugging - use bright colors for visibility
            this.renderer.domElement.style.border = '3px solid yellow';
            this.renderer.domElement.style.backgroundColor = 'rgba(255, 255, 0, 0.3)';
            
            console.log('Renderer added to DOM');
            console.log('Canvas dimensions:', this.renderer.domElement.width, 'x', this.renderer.domElement.height);
            console.log('Canvas style:', this.renderer.domElement.style.cssText);
            console.log('Canvas position:', this.renderer.domElement.offsetLeft, this.renderer.domElement.offsetTop);
            console.log('Canvas computed style:', window.getComputedStyle(this.renderer.domElement).zIndex);
            
            // Force a repaint and check if canvas is visible
            setTimeout(() => {
                console.log('Canvas visibility check after 100ms:');
                console.log('Canvas offset:', this.renderer.domElement.offsetLeft, this.renderer.domElement.offsetTop);
                console.log('Canvas size:', this.renderer.domElement.offsetWidth, 'x', this.renderer.domElement.offsetHeight);
                console.log('Canvas z-index:', window.getComputedStyle(this.renderer.domElement).zIndex);
                console.log('Canvas display:', window.getComputedStyle(this.renderer.domElement).display);
                console.log('Canvas visibility:', window.getComputedStyle(this.renderer.domElement).visibility);
                
                // If canvas is not visible, try to force it
                if (this.renderer.domElement.offsetWidth === 0 || this.renderer.domElement.offsetHeight === 0) {
                    console.log('Canvas not visible, forcing dimensions...');
                    this.renderer.domElement.style.width = '100vw';
                    this.renderer.domElement.style.height = '100vh';
                    this.renderer.setSize(window.innerWidth, window.innerHeight);
                }
            }, 100);
            
            // Create full-screen quad - make it larger to cover the screen
            const geometry = new THREE.PlaneGeometry(4, 4);
            console.log('Geometry created: 4x4 plane');
            
            // Start with a simple material for testing - use a bright color for visibility
            this.material = new THREE.MeshBasicMaterial({ 
                color: 0x00ff00, // Bright green for better visibility
                transparent: true,
                opacity: 1.0     // Full opacity for testing
            });
            console.log('Material created with green color and full opacity');
            
            this.mesh = new THREE.Mesh(geometry, this.material);
            this.scene.add(this.mesh);
            console.log('Mesh added to scene');
            
            // Test render immediately
            this.renderer.render(this.scene, this.camera);
            console.log('Test render completed');
            
            // Force a second render to ensure it's working
            setTimeout(() => {
                this.renderer.render(this.scene, this.camera);
                console.log('Second test render completed');
            }, 100);
            
            console.log('WebGL scene initialization complete');
        } catch (error) {
            console.error('Error in init method:', error);
            // Add fallback background
            document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            
            // Also try creating a simple colored div as fallback
            try {
                const fallbackDiv = document.createElement('div');
                fallbackDiv.style.position = 'fixed';
                fallbackDiv.style.top = '0';
                fallbackDiv.style.left = '0';
                fallbackDiv.style.width = '100%';
                fallbackDiv.style.height = '100%';
                fallbackDiv.style.zIndex = '-1';
                fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
                document.body.insertBefore(fallbackDiv, document.body.firstChild);
                console.log('Fallback div created');
            } catch (fallbackError) {
                console.error('Failed to create fallback div:', fallbackError);
            }
        } catch (error) {
            console.error('Error in init method:', error);
            // Add fallback background
            document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            
            // Also try creating a simple colored div as fallback
            try {
                const fallbackDiv = document.createElement('div');
                fallbackDiv.style.position = 'fixed';
                fallbackDiv.style.top = '0';
                fallbackDiv.style.left = '0';
                fallbackDiv.style.width = '100%';
                fallbackDiv.style.height = '100%';
                fallbackDiv.style.zIndex = '-1';
                fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
                document.body.insertBefore(fallbackDiv, document.body.firstChild);
                console.log('Fallback div created');
            } catch (fallbackError) {
                console.error('Failed to create fallback div:', fallbackError);
            }
        }
    }
    
    setupEventListeners() {
        // Window resize only for now
        window.addEventListener('resize', () => {
            if (this.camera && this.renderer) {
                this.camera.aspect = window.innerWidth / window.innerHeight;
                this.camera.updateProjectionMatrix();
                this.renderer.setSize(window.innerWidth, window.innerHeight);
            }
        });
    }
    
    animate() {
        try {
            requestAnimationFrame(() => this.animate());
            
            const elapsedTime = this.clock.getElapsedTime();
            
            // Simple animation for testing
            if (this.mesh && this.mesh.material.color) {
                this.mesh.material.color.setHSL(elapsedTime * 0.1, 0.5, 0.5);
            }
            
            // Render
            this.renderer.render(this.scene, this.camera);
            
            // Log first few frames for debugging
            if (Math.floor(elapsedTime * 60) < 10) {
                console.log(`Frame rendered at ${elapsedTime.toFixed(2)}s`);
            }
        } catch (error) {
            console.error('Error in animate method:', error);
            // Add fallback background
            document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            
            // Also create a fallback div
            try {
                const fallbackDiv = document.createElement('div');
                fallbackDiv.style.position = 'fixed';
                fallbackDiv.style.top = '0';
                fallbackDiv.style.left = '0';
                fallbackDiv.style.width = '100%';
                fallbackDiv.style.height = '100%';
                fallbackDiv.style.zIndex = '-1';
                fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
                document.body.insertBefore(fallbackDiv, document.body.firstChild);
                console.log('Animate error fallback div created');
            } catch (fallbackError) {
                console.error('Failed to create animate fallback div:', fallbackError);
            }
        }
    }
}

// Initialize WebGL background when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    try {
        console.log('DOM loaded, checking Three.js...');
        
        // TEMPORARILY DISABLE WEBGL FOR SCROLL TESTING
        console.log('WebGL temporarily disabled for scroll testing');
        document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
        return;
        
        // Check if Three.js is loaded
        if (typeof THREE === 'undefined') {
            console.error('Three.js not loaded!');
            // Add fallback background immediately
            document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            
            // Also create a fallback div
            const fallbackDiv = document.createElement('div');
            fallbackDiv.style.position = 'fixed';
            fallbackDiv.style.top = '0';
            fallbackDiv.style.left = '0';
            fallbackDiv.style.width = '100%';
            fallbackDiv.style.height = '100%';
            fallbackDiv.style.zIndex = '-1';
            fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            document.body.insertBefore(fallbackDiv, document.body.firstChild);
            console.log('Three.js fallback div created');
            return;
        }
        
        console.log('Three.js loaded, version:', THREE.REVISION);
        
        // Check WebGL support
        if (typeof THREE.WEBGL !== 'undefined' && !THREE.WEBGL.isWebGLAvailable()) {
            console.error('WebGL not available!');
            // Add fallback background immediately
            document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            
            // Also create a fallback div
            const fallbackDiv = document.createElement('div');
            fallbackDiv.style.position = 'fixed';
            fallbackDiv.style.top = '0';
            fallbackDiv.style.left = '0';
            fallbackDiv.style.width = '100%';
            fallbackDiv.style.height = '100%';
            fallbackDiv.style.zIndex = '-1';
            fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            document.body.insertBefore(fallbackDiv, document.body.firstChild);
            console.log('WebGL availability fallback div created');
            return;
        }
        
        // Alternative WebGL detection
        const canvas = document.createElement('canvas');
        const gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
        if (!gl) {
            console.error('WebGL not supported by browser!');
            // Add fallback background
            document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            
            // Also create a fallback div
            const fallbackDiv = document.createElement('div');
            fallbackDiv.style.position = 'fixed';
            fallbackDiv.style.top = '0';
            fallbackDiv.style.left = '0';
            fallbackDiv.style.width = '100%';
            fallbackDiv.style.height = '100%';
            fallbackDiv.style.zIndex = '-1';
            fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            document.body.insertBefore(fallbackDiv, document.body.firstChild);
            console.log('WebGL fallback div created');
            return;
        }
        
        console.log('WebGL context created successfully');
        console.log('Creating WebGLBackground...');
        
        const background = new WebGLBackground();
        
        console.log('WebGLBackground created successfully');
        
        // Add a timeout to check if WebGL is working after a delay
        setTimeout(() => {
            if (background && background.renderer && background.renderer.domElement) {
                const canvas = background.renderer.domElement;
                console.log('WebGL status check after 1 second:');
                console.log('Canvas visible:', canvas.offsetWidth > 0 && canvas.offsetHeight > 0);
                console.log('Canvas position:', canvas.offsetLeft, canvas.offsetTop);
                console.log('Canvas z-index:', window.getComputedStyle(canvas).zIndex);
                
                // If canvas is not visible, try to force it
                if (canvas.offsetWidth === 0 || canvas.offsetHeight === 0) {
                    console.log('Canvas not visible, forcing dimensions...');
                    canvas.style.width = '100vw';
                    canvas.style.height = '100vh';
                    canvas.width = window.innerWidth;
                    canvas.height = window.innerHeight;
                }
            }
        }, 1000);
        
    } catch (error) {
        console.error('Failed to initialize WebGL Background:', error);
        // Add fallback background
        document.body.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
        
        // Also create a fallback div
        try {
            const fallbackDiv = document.createElement('div');
            fallbackDiv.style.position = 'fixed';
            fallbackDiv.style.top = '0';
            fallbackDiv.style.left = '0';
            fallbackDiv.style.width = '100%';
            fallbackDiv.style.height = '100%';
            fallbackDiv.style.zIndex = '-1';
            fallbackDiv.style.background = 'linear-gradient(135deg, #0f0f23 0%, #1a1a2e 50%, #16213e 100%)';
            document.body.insertBefore(fallbackDiv, document.body.firstChild);
            console.log('Main error fallback div created');
        } catch (fallbackError) {
            console.error('Failed to create main fallback div:', fallbackError);
        }
    }
});
