#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

// Build script for psyop.ca website
console.log('Building psyop.ca website...');

// Create dist directory
const distDir = path.join(__dirname, '..', 'dist');
if (!fs.existsSync(distDir)) {
    fs.mkdirSync(distDir, { recursive: true });
}

// Create subdirectories
const subdirs = ['styles', 'assets', 'assets/images', 'assets/audio'];
subdirs.forEach(dir => {
    const fullPath = path.join(distDir, dir);
    if (!fs.existsSync(fullPath)) {
        fs.mkdirSync(fullPath, { recursive: true });
    }
});

// Copy HTML files
const srcDir = path.join(__dirname, '..', 'src');
const htmlFiles = fs.readdirSync(srcDir).filter(file => file.endsWith('.html'));

htmlFiles.forEach(file => {
    const srcPath = path.join(srcDir, file);
    const destPath = path.join(distDir, file);
    fs.copyFileSync(srcPath, destPath);
    console.log(`Copied ${file} to dist/`);
});

// Copy assets
const assetsDir = path.join(srcDir, 'assets');
if (fs.existsSync(assetsDir)) {
    copyRecursive(assetsDir, path.join(distDir, 'assets'));
}

console.log('Build completed successfully!');

function copyRecursive(src, dest) {
    const stats = fs.statSync(src);
    
    if (stats.isDirectory()) {
        if (!fs.existsSync(dest)) {
            fs.mkdirSync(dest, { recursive: true });
        }
        
        fs.readdirSync(src).forEach(file => {
            copyRecursive(path.join(src, file), path.join(dest, file));
        });
    } else {
        fs.copyFileSync(src, dest);
        console.log(`Copied asset: ${path.relative(process.cwd(), dest)}`);
    }
}
