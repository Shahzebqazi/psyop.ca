// PSYOP Website - WebGL Background
// Dynamic background with animated particles and effects

class WebGLBackground {
    constructor() {
        this.canvas = document.getElementById('webgl-background');
        this.gl = null;
        this.program = null;
        this.particles = [];
        this.particleCount = 100;
        this.time = 0;
        this.mouseX = 0;
        this.mouseY = 0;
        
        this.init();
    }
    
    init() {
        if (!this.canvas) return;
        
        // Get WebGL context
        this.gl = this.canvas.getContext('webgl') || this.canvas.getContext('experimental-webgl');
        if (!this.gl) {
            console.warn('WebGL not supported, falling back to CSS background');
            this.fallbackBackground();
            return;
        }
        
        this.setupCanvas();
        this.createShaders();
        this.createBuffers();
        this.createParticles();
        this.bindEvents();
        this.render();
    }
    
    setupCanvas() {
        // Set canvas size
        this.resizeCanvas();
        
        // Set viewport
        this.gl.viewport(0, 0, this.gl.drawingBufferWidth, this.gl.drawingBufferHeight);
        
        // Clear color
        this.gl.clearColor(0.0, 0.0, 0.0, 1.0);
        this.gl.enable(this.gl.BLEND);
        this.gl.blendFunc(this.gl.SRC_ALPHA, this.gl.ONE_MINUS_SRC_ALPHA);
    }
    
    resizeCanvas() {
        const rect = this.canvas.getBoundingClientRect();
        const pixelRatio = window.devicePixelRatio || 1;
        
        this.canvas.width = rect.width * pixelRatio;
        this.canvas.height = rect.height * pixelRatio;
        
        this.gl.viewport(0, 0, this.gl.drawingBufferWidth, this.gl.drawingBufferHeight);
    }
    
    createShaders() {
        // Vertex shader
        const vertexShaderSource = `
            attribute vec2 a_position;
            attribute float a_size;
            attribute vec3 a_color;
            attribute float a_alpha;
            
            uniform float u_time;
            uniform vec2 u_resolution;
            uniform vec2 u_mouse;
            
            varying vec3 v_color;
            varying float v_alpha;
            
            void main() {
                vec2 position = a_position;
                
                // Add some movement based on time
                position.x += sin(u_time * 0.5 + a_position.y * 0.1) * 0.1;
                position.y += cos(u_time * 0.3 + a_position.x * 0.1) * 0.1;
                
                // Convert to clip space
                vec2 clipSpace = (position / u_resolution) * 2.0 - 1.0;
                
                gl_Position = vec4(clipSpace, 0.0, 1.0);
                gl_PointSize = a_size * (1.0 + sin(u_time + a_position.x) * 0.2);
                
                v_color = a_color;
                v_alpha = a_alpha;
            }
        `;
        
        // Fragment shader
        const fragmentShaderSource = `
            precision mediump float;
            
            varying vec3 v_color;
            varying float v_alpha;
            
            void main() {
                vec2 center = gl_PointCoord - vec2(0.5);
                float dist = length(center);
                
                if (dist > 0.5) {
                    discard;
                }
                
                float alpha = (1.0 - dist * 2.0) * v_alpha;
                gl_FragColor = vec4(v_color, alpha);
            }
        `;
        
        // Create and compile shaders
        const vertexShader = this.createShader(this.gl.VERTEX_SHADER, vertexShaderSource);
        const fragmentShader = this.createShader(this.gl.FRAGMENT_SHADER, fragmentShaderSource);
        
        // Create program
        this.program = this.createProgram(vertexShader, fragmentShader);
        this.gl.useProgram(this.program);
    }
    
    createShader(type, source) {
        const shader = this.gl.createShader(type);
        this.gl.shaderSource(shader, source);
        this.gl.compileShader(shader);
        
        if (!this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS)) {
            console.error('Shader compilation error:', this.gl.getShaderInfoLog(shader));
            this.gl.deleteShader(shader);
            return null;
        }
        
        return shader;
    }
    
    createProgram(vertexShader, fragmentShader) {
        const program = this.gl.createProgram();
        this.gl.attachShader(program, vertexShader);
        this.gl.attachShader(program, fragmentShader);
        this.gl.linkProgram(program);
        
        if (!this.gl.getProgramParameter(program, this.gl.LINK_STATUS)) {
            console.error('Program linking error:', this.gl.getProgramInfoLog(program));
            this.gl.deleteProgram(program);
            return null;
        }
        
        return program;
    }
    
    createBuffers() {
        // Position buffer
        this.positionBuffer = this.gl.createBuffer();
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.positionBuffer);
        
        // Size buffer
        this.sizeBuffer = this.gl.createBuffer();
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.sizeBuffer);
        
        // Color buffer
        this.colorBuffer = this.gl.createBuffer();
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.colorBuffer);
        
        // Alpha buffer
        this.alphaBuffer = this.gl.createBuffer();
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.alphaBuffer);
    }
    
    createParticles() {
        this.particles = [];
        
        for (let i = 0; i < this.particleCount; i++) {
            this.particles.push({
                x: (Math.random() - 0.5) * 2,
                y: (Math.random() - 0.5) * 2,
                size: Math.random() * 3 + 1,
                color: [
                    Math.random() * 0.3 + 0.7, // Red component
                    Math.random() * 0.2,        // Green component
                    Math.random() * 0.2         // Blue component
                ],
                alpha: Math.random() * 0.5 + 0.1,
                speed: Math.random() * 0.02 + 0.01
            });
        }
    }
    
    bindEvents() {
        // Handle window resize
        window.addEventListener('resize', () => {
            this.resizeCanvas();
        });
        
        // Handle mouse movement
        document.addEventListener('mousemove', (e) => {
            this.mouseX = (e.clientX / window.innerWidth) * 2 - 1;
            this.mouseY = (e.clientY / window.innerHeight) * 2 - 1;
        });
        
        // Handle touch movement
        document.addEventListener('touchmove', (e) => {
            if (e.touches.length > 0) {
                this.mouseX = (e.touches[0].clientX / window.innerWidth) * 2 - 1;
                this.mouseY = (e.touches[0].clientY / window.innerHeight) * 2 - 1;
            }
        });
    }
    
    updateParticles() {
        this.time += 0.016; // Assuming 60fps
        
        // Update particle positions
        this.particles.forEach(particle => {
            particle.x += Math.sin(this.time * particle.speed) * 0.001;
            particle.y += Math.cos(this.time * particle.speed) * 0.001;
            
            // Wrap around edges
            if (particle.x > 1) particle.x = -1;
            if (particle.x < -1) particle.x = 1;
            if (particle.y > 1) particle.y = -1;
            if (particle.y < -1) particle.y = 1;
        });
    }
    
    render() {
        if (!this.gl || !this.program) return;
        
        this.updateParticles();
        
        // Clear canvas
        this.gl.clear(this.gl.COLOR_BUFFER_BIT);
        
        // Get uniform locations
        const timeLocation = this.gl.getUniformLocation(this.program, 'u_time');
        const resolutionLocation = this.gl.getUniformLocation(this.program, 'u_resolution');
        const mouseLocation = this.gl.getUniformLocation(this.program, 'u_mouse');
        
        // Set uniforms
        this.gl.uniform1f(timeLocation, this.time);
        this.gl.uniform2f(resolutionLocation, this.gl.drawingBufferWidth, this.gl.drawingBufferHeight);
        this.gl.uniform2f(mouseLocation, this.mouseX, this.mouseY);
        
        // Prepare data arrays
        const positions = new Float32Array(this.particles.length * 2);
        const sizes = new Float32Array(this.particles.length);
        const colors = new Float32Array(this.particles.length * 3);
        const alphas = new Float32Array(this.particles.length);
        
        this.particles.forEach((particle, i) => {
            positions[i * 2] = particle.x;
            positions[i * 2 + 1] = particle.y;
            sizes[i] = particle.size;
            colors[i * 3] = particle.color[0];
            colors[i * 3 + 1] = particle.color[1];
            colors[i * 3 + 2] = particle.color[2];
            alphas[i] = particle.alpha;
        });
        
        // Bind and update buffers
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.positionBuffer);
        this.gl.bufferData(this.gl.ARRAY_BUFFER, positions, this.gl.DYNAMIC_DRAW);
        
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.sizeBuffer);
        this.gl.bufferData(this.gl.ARRAY_BUFFER, sizes, this.gl.DYNAMIC_DRAW);
        
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.colorBuffer);
        this.gl.bufferData(this.gl.ARRAY_BUFFER, colors, this.gl.DYNAMIC_DRAW);
        
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.alphaBuffer);
        this.gl.bufferData(this.gl.ARRAY_BUFFER, alphas, this.gl.DYNAMIC_DRAW);
        
        // Get attribute locations
        const positionLocation = this.gl.getAttribLocation(this.program, 'a_position');
        const sizeLocation = this.gl.getAttribLocation(this.program, 'a_size');
        const colorLocation = this.gl.getAttribLocation(this.program, 'a_color');
        const alphaLocation = this.gl.getAttribLocation(this.program, 'a_alpha');
        
        // Enable attributes
        this.gl.enableVertexAttribArray(positionLocation);
        this.gl.enableVertexAttribArray(sizeLocation);
        this.gl.enableVertexAttribArray(colorLocation);
        this.gl.enableVertexAttribArray(alphaLocation);
        
        // Bind attributes
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.positionBuffer);
        this.gl.vertexAttribPointer(positionLocation, 2, this.gl.FLOAT, false, 0, 0);
        
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.sizeBuffer);
        this.gl.vertexAttribPointer(sizeLocation, 1, this.gl.FLOAT, false, 0, 0);
        
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.colorBuffer);
        this.gl.vertexAttribPointer(colorLocation, 3, this.gl.FLOAT, false, 0, 0);
        
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.alphaBuffer);
        this.gl.vertexAttribPointer(alphaLocation, 1, this.gl.FLOAT, false, 0, 0);
        
        // Draw particles
        this.gl.drawArrays(this.gl.POINTS, 0, this.particles.length);
        
        // Request next frame
        requestAnimationFrame(() => this.render());
    }
    
    fallbackBackground() {
        // Fallback to CSS background if WebGL is not supported
        this.canvas.style.display = 'none';
        document.body.style.background = 'linear-gradient(135deg, #000000 0%, #1a1a1a 50%, #000000 100%)';
    }
}

// Initialize WebGL background when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new WebGLBackground();
});

// Export for potential external use
window.WebGLBackground = WebGLBackground;
