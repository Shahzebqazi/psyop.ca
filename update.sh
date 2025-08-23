#!/bin/bash

# Update and deploy script for psyop.ca
# This script pulls the latest changes and deploys them

set -e

echo "🔄 Starting update and deploy at $(date)" >> /var/log/psyop/server.log

# Pull latest changes
echo "📥 Pulling latest changes from GitHub..." >> /var/log/psyop/server.log
if git pull origin main; then
    echo "✅ Git pull successful at $(date)" >> /var/log/psyop/server.log
    
    # Deploy the changes
    echo "🚀 Deploying changes..." >> /var/log/psyop/server.log
    ./deploy.sh
    
    echo "🎉 Update and deploy completed successfully at $(date)" >> /var/log/psyop/server.log
else
    echo "❌ Update and deploy aborted due to git pull failure" >> /var/log/psyop/server.log
    exit 1
fi
