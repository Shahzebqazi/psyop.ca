#!/bin/bash

# PSYOP Website - Environment Variable Loader
# This script loads environment variables from .env file

# Check if .env file exists
if [ -f ".env" ]; then
    echo "Loading environment variables from .env file..."
    
    # Export all variables from .env file
    export $(cat .env | grep -v '^#' | xargs)
    
    echo "Environment variables loaded successfully"
    echo "Current configuration:"
    echo "  APP_NAME: ${APP_NAME:-not set}"
    echo "  DOMAIN_NAME: ${DOMAIN_NAME:-not set}"
    echo "  ADMIN_EMAIL: ${ADMIN_EMAIL:-not set}"
    echo "  GITHUB_USERNAME: ${GITHUB_USERNAME:-not set}"
    echo "  GITHUB_REPO: ${GITHUB_REPO:-not set}"
    echo "  REPO_URL: ${REPO_URL:-not set}"
else
    echo "No .env file found. Using default values."
    echo "Copy env.template to .env and configure your values."
fi

# Function to show current environment
show_env() {
    echo "Current environment variables:"
    env | grep -E "^(APP_|DOMAIN_|ADMIN_|GITHUB_|REPO_|WEBHOOK_|SSL_)" | sort
}

# Function to validate required variables
validate_env() {
    local missing_vars=()
    
    # Check for required variables
    [ -z "$ADMIN_EMAIL" ] && missing_vars+=("ADMIN_EMAIL")
    [ -z "$DOMAIN_NAME" ] && missing_vars+=("DOMAIN_NAME")
    [ -z "$GITHUB_USERNAME" ] && missing_vars+=("GITHUB_USERNAME")
    [ -z "$GITHUB_REPO" ] && missing_vars+=("GITHUB_REPO")
    
    if [ ${#missing_vars[@]} -eq 0 ]; then
        echo "✅ All required environment variables are set"
        return 0
    else
        echo "❌ Missing required environment variables:"
        printf '  - %s\n' "${missing_vars[@]}"
        echo ""
        echo "Please set these variables in your .env file"
        return 1
    fi
}

# Function to create .env from template
create_env() {
    if [ -f "env.template" ]; then
        cp env.template .env
        echo "✅ Created .env file from template"
        echo "Please edit .env with your actual values"
    else
        echo "❌ env.template not found"
    fi
}

# Main execution
case "${1:-}" in
    show)
        show_env
        ;;
    validate)
        validate_env
        ;;
    create)
        create_env
        ;;
    *)
        # Load environment by default
        if [ -f ".env" ]; then
            export $(cat .env | grep -v '^#' | xargs)
        fi
        ;;
esac
