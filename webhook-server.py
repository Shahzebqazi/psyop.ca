#!/usr/bin/env python3
"""
PSYOP Website - Webhook Server
Automatically deploys the website when code is pushed to the main branch
"""

import os
import json
import hmac
import hashlib
import subprocess
import logging
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import parse_qs, urlparse
import threading
import time

# Configuration
WEBHOOK_SECRET = os.getenv('WEBHOOK_SECRET', 'your-webhook-secret-here')
WEBHOOK_PORT = int(os.getenv('WEBHOOK_PORT', '9000'))
DEPLOY_SCRIPT = os.getenv('DEPLOY_SCRIPT', './deploy-digitalocean.sh')
REPO_PATH = os.getenv('REPO_PATH', '/opt/psyop-website')
LOG_FILE = os.getenv('LOG_FILE', '/var/log/psyop-webhook.log')

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler(LOG_FILE),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class WebhookHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        """Handle POST requests from GitHub webhooks"""
        try:
            # Get content length and read body
            content_length = int(self.headers.get('Content-Length', 0))
            body = self.rfile.read(content_length)
            
            # Verify webhook signature
            if not self.verify_signature(body):
                logger.warning("Invalid webhook signature")
                self.send_response(401)
                self.end_headers()
                return
            
            # Parse JSON payload
            payload = json.loads(body.decode('utf-8'))
            
            # Check if this is a push to main branch
            if self.is_push_to_main(payload):
                logger.info("Push to main branch detected, triggering deployment")
                
                # Send immediate response
                self.send_response(200)
                self.end_headers()
                self.wfile.write(b'Deployment triggered')
                
                # Trigger deployment in background
                threading.Thread(target=self.trigger_deployment).start()
            else:
                logger.info("Push to non-main branch, ignoring")
                self.send_response(200)
                self.end_headers()
                self.wfile.write(b'Ignored - not main branch')
                
        except Exception as e:
            logger.error(f"Error processing webhook: {e}")
            self.send_response(500)
            self.end_headers()
            self.wfile.write(b'Internal server error')
    
    def verify_signature(self, body):
        """Verify GitHub webhook signature"""
        signature = self.headers.get('X-Hub-Signature-256', '')
        if not signature.startswith('sha256='):
            return False
        
        expected_signature = 'sha256=' + hmac.new(
            WEBHOOK_SECRET.encode('utf-8'),
            body,
            hashlib.sha256
        ).hexdigest()
        
        return hmac.compare_digest(signature, expected_signature)
    
    def is_push_to_main(self, payload):
        """Check if this is a push to the main branch"""
        try:
            ref = payload.get('ref', '')
            return ref == 'refs/heads/main'
        except:
            return False
    
    def trigger_deployment(self):
        """Trigger the deployment process"""
        try:
            logger.info("Starting deployment process")
            
            # Pull latest changes
            logger.info("Pulling latest changes from git")
            subprocess.run(['git', 'pull', 'origin', 'main'], 
                         cwd=REPO_PATH, check=True, capture_output=True)
            
            # Build the application
            logger.info("Building Haskell application")
            subprocess.run(['stack', 'build'], 
                         cwd=REPO_PATH, check=True, capture_output=True)
            
            # Restart the service
            logger.info("Restarting psyop-website service")
            subprocess.run(['sudo', 'systemctl', 'restart', 'psyop-website'], 
                         check=True, capture_output=True)
            
            logger.info("Deployment completed successfully")
            
        except subprocess.CalledProcessError as e:
            logger.error(f"Deployment failed: {e}")
            logger.error(f"stdout: {e.stdout.decode() if e.stdout else 'N/A'}")
            logger.error(f"stderr: {e.stderr.decode() if e.stderr else 'N/A'}")
        except Exception as e:
            logger.error(f"Unexpected error during deployment: {e}")
    
    def log_message(self, format, *args):
        """Override to use our logger"""
        logger.info(f"{self.address_string()} - {format % args}")

def main():
    """Main function to start the webhook server"""
    try:
        # Create server
        server = HTTPServer(('0.0.0.0', WEBHOOK_PORT), WebhookHandler)
        logger.info(f"Starting webhook server on port {WEBHOOK_PORT}")
        logger.info(f"Webhook secret: {WEBHOOK_SECRET[:8]}...")
        logger.info(f"Deploy script: {DEPLOY_SCRIPT}")
        logger.info(f"Repository path: {REPO_PATH}")
        
        # Start server
        server.serve_forever()
        
    except KeyboardInterrupt:
        logger.info("Shutting down webhook server")
        server.shutdown()
    except Exception as e:
        logger.error(f"Failed to start webhook server: {e}")

if __name__ == '__main__':
    main()
