#!/usr/bin/env node

const sharp = require('sharp');
const fs = require('fs');
const path = require('path');

async function optimizeImage() {
  const inputPath = 'static/single_spotify_soundcloud_bandcamp.jpg';
  const outputPath = 'static/single_spotify_soundcloud_bandcamp_optimized.jpg';
  const backupPath = 'static/single_spotify_soundcloud_bandcamp_original.jpg';
  
  try {
    // Check if input file exists
    if (!fs.existsSync(inputPath)) {
      console.error(`Input file not found: ${inputPath}`);
      process.exit(1);
    }
    
    // Get original file stats
    const originalStats = fs.statSync(inputPath);
    const originalSizeKB = Math.round(originalStats.size / 1024);
    
    console.log(`Original image: ${originalSizeKB}KB`);
    
    // Create backup of original
    if (!fs.existsSync(backupPath)) {
      fs.copyFileSync(inputPath, backupPath);
      console.log(`Backup created: ${backupPath}`);
    }
    
    // Get image metadata
    const metadata = await sharp(inputPath).metadata();
    console.log(`Original dimensions: ${metadata.width}x${metadata.height}`);
    
    // Optimize the image
    // Resize to reasonable web size (800x800 for album art)
    // Use progressive JPEG with quality 85
    await sharp(inputPath)
      .resize(800, 800, {
        fit: 'cover',
        position: 'center'
      })
      .jpeg({
        quality: 85,
        progressive: true,
        mozjpeg: true
      })
      .toFile(outputPath);
    
    // Get optimized file stats
    const optimizedStats = fs.statSync(outputPath);
    const optimizedSizeKB = Math.round(optimizedStats.size / 1024);
    const compressionRatio = Math.round((1 - optimizedStats.size / originalStats.size) * 100);
    
    console.log(`Optimized image: ${optimizedSizeKB}KB (${compressionRatio}% smaller)`);
    console.log(`Optimized dimensions: 800x800`);
    
    // Replace original with optimized version
    fs.renameSync(outputPath, inputPath);
    console.log(`✅ Image optimization complete!`);
    console.log(`📊 Size reduction: ${originalSizeKB}KB → ${optimizedSizeKB}KB`);
    
  } catch (error) {
    console.error('Error optimizing image:', error);
    process.exit(1);
  }
}

// Generate multiple sizes for responsive images
async function generateResponsiveImages() {
  const inputPath = 'static/single_spotify_soundcloud_bandcamp.jpg';
  const sizes = [
    { width: 400, suffix: '_400' },
    { width: 600, suffix: '_600' },
    { width: 800, suffix: '_800' },
    { width: 1200, suffix: '_1200' }
  ];
  
  try {
    for (const size of sizes) {
      const outputPath = `static/single_spotify_soundcloud_bandcamp${size.suffix}.jpg`;
      
      await sharp(inputPath)
        .resize(size.width, size.width, {
          fit: 'cover',
          position: 'center'
        })
        .jpeg({
          quality: 85,
          progressive: true,
          mozjpeg: true
        })
        .toFile(outputPath);
      
      const stats = fs.statSync(outputPath);
      const sizeKB = Math.round(stats.size / 1024);
      console.log(`Generated ${size.width}x${size.width}: ${sizeKB}KB`);
    }
    
    console.log('✅ Responsive images generated!');
    
  } catch (error) {
    console.error('Error generating responsive images:', error);
    process.exit(1);
  }
}

async function main() {
  console.log('🖼️  Optimizing PSYOP album art...');
  await optimizeImage();
  
  console.log('\n📱 Generating responsive image sizes...');
  await generateResponsiveImages();
}

if (require.main === module) {
  main();
}

module.exports = { optimizeImage, generateResponsiveImages };
