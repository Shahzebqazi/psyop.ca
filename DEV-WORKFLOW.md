# Development Workflow Guide

## 🚀 Current Status

You are now working on the **`dev`** branch. This is your development workspace where you can make changes without affecting the production website.

## 📋 Branch Structure

- **`main`** → Production branch (auto-deploys to website)
- **`dev`** → Development branch (your current workspace)
- **`docker_k8s`** → Docker/Kubernetes configurations
- **`test-build-1`** → Legacy site implementations

## 🔄 Development Workflow

### 1. **Make Changes** (You are here)
```bash
# You're on dev branch - make your changes
# Edit files, add features, fix bugs, etc.
```

### 2. **Stage Changes**
```bash
# Add specific files
git add filename.ext

# Add all changes
git add .

# Check what's staged
git status
```

### 3. **Commit Changes**
```bash
# Commit with descriptive message
git commit -m "Add new feature: user authentication"

# Or commit with more details
git commit -m "Fix bug: resolve navigation menu issue

- Updated navigation component
- Fixed mobile responsiveness
- Added error handling"
```

### 4. **Push to Dev Branch**
```bash
# Push your changes to remote dev branch
git push origin dev
```

### 5. **When Ready for Production**
```bash
# Switch to main branch
git checkout main

# Pull latest changes
git pull origin main

# Merge dev branch into main
git merge dev

# Push to main (triggers auto-deployment)
git push origin main

# Switch back to dev for continued development
git checkout dev
```

## 🛠️ Useful Commands

### **Branch Management**
```bash
# See all branches
git branch -a

# See current branch
git branch

# Switch between branches
git checkout branch-name

# Create and switch to new feature branch
git checkout -b feature-name
```

### **Status and History**
```bash
# Check current status
git status

# See commit history
git log --oneline

# See changes in last commit
git show

# See differences
git diff
```

### **Stashing (Save work in progress)**
```bash
# Save current work without committing
git stash

# List stashes
git stash list

# Apply last stash
git stash pop

# Apply specific stash
git stash apply stash@{n}
```

## 📁 File Organization

### **Development Files**
- **`src/`** → Haskell source code
- **`static/`** → CSS, images, static assets
- **`app/`** → Main application entry point
- **`test/`** → Test files
- **`*.md`** → Documentation

### **Deployment Files**
- **`auto-deploy.sh`** → Main deployment script
- **`webhook-server.py`** → GitHub webhook receiver
- **`server-setup.sh`** → Server preparation
- **`DEPLOYMENT.md`** → Deployment documentation

## 🚨 Important Notes

### **Never Work Directly on Main**
- Main branch auto-deploys to production
- Always develop on `dev` or feature branches
- Only merge to main when ready for production

### **Commit Messages**
- Use clear, descriptive commit messages
- Reference issues/features when applicable
- Keep commits focused and logical

### **Testing**
- Test your changes locally before pushing
- Run `stack test` to ensure tests pass
- Check that the application builds successfully

## 🔍 Development Tips

### **Local Testing**
```bash
# Build the application
stack build

# Run tests
stack test

# Start local server
stack run
```

### **Code Quality**
- Follow Haskell best practices
- Use meaningful variable names
- Add comments for complex logic
- Keep functions small and focused

### **Git Best Practices**
- Commit frequently with small, logical changes
- Use descriptive branch names
- Pull before pushing to avoid conflicts
- Review changes before committing

## 🚀 Quick Start Commands

```bash
# You're already on dev branch, so just:

# 1. Make your changes to files

# 2. Stage changes
git add .

# 3. Commit
git commit -m "Your commit message"

# 4. Push to dev
git push origin dev

# 5. Continue developing...
```

## 📊 Current Project Status

- ✅ **Dev branch created and active**
- ✅ **All deployment scripts ready**
- ✅ **Real-time deployment system configured**
- ✅ **Ready for development work**

---

**You're all set!** Work on the `dev` branch, commit your changes, and when you're ready, merge to `main` to deploy to production. The website will automatically update within 30-60 seconds after you push to main.
