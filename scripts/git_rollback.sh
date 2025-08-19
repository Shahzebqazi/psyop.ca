#!/bin/bash

# ==============================================================================
# Git Rollback Script for CI/CD Pipeline
# This script uses Git to roll back the live website to the previous commit.
#
# USAGE: ./git_rollback.sh
# ==============================================================================

# --- Configuration ---
# Set the path to your live website deployment directory, which must be a Git repository.
DEPLOYMENT_DIR="/var/www/html"

# ==============================================================================
# --- Script Logic (DO NOT MODIFY BELOW THIS LINE) ---
# ==============================================================================

echo "Starting website rollback using Git..."

# A safety check to make sure the target directory exists and is a Git repository.
if [ ! -d "$DEPLOYMENT_DIR" ] || [ ! -d "$DEPLOYMENT_DIR/.git" ]; then
    echo "ERROR: Deployment directory '$DEPLOYMENT_DIR' is not a Git repository. Rollback aborted."
    exit 1
fi

# ==============================================================================
# --- THE ROLLBACK ACTION ---
# This command performs a hard reset, which forces the repository to a previous commit.
# `HEAD^` refers to the commit just before the current HEAD (the previous commit).
# The `--hard` flag ensures that the working directory and index are also updated.
# This action will permanently remove all uncommitted changes.
# ==============================================================================
echo "Rolling back to the previous commit..."
cd "$DEPLOYMENT_DIR" || exit 1
git reset --hard HEAD^

# A sanity check to show the current commit after the rollback.
echo "✅ Rollback complete. The repository is now at commit:"
git log -1 --oneline

# ==============================================================================
# --- CLEANUP (OPTIONAL BUT RECOMMENDED) ---
# Clean up any untracked files or directories that may have been created
# during the failed deployment.
# ==============================================================================
echo "Cleaning up any untracked files..."
git clean -df

echo "Please verify the website is working correctly."
