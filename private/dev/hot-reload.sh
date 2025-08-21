#!/bin/bash

echo "🔥 PSYOP Hot Reloading Development Server"
echo "🧭 Testing MenuBar Component Architecture"
echo "Starting development environment with file watching..."

echo "🚀 Starting server with automatic restart on file changes..."
echo "💡 The server will automatically restart when you save changes!"
echo "🌐 Server will be available at http://localhost:8080"
echo "🧭 Testing MenuBar component only - simplified routing"
echo "📁 Watching src/ and src/components/ for changes"
echo "Press Ctrl+C to stop"

# Function to start server
start_server() {
    echo "🔄 Starting server..."
    cd "$(dirname "$0")/../.."  # Navigate to project root
    stack run &
    SERVER_PID=$!
    echo "✅ Server started with PID: $SERVER_PID"
    cd - > /dev/null  # Return to script directory
}

# Function to stop server
stop_server() {
    if [ ! -z "$SERVER_PID" ]; then
        echo "🛑 Stopping server (PID: $SERVER_PID)..."
        kill $SERVER_PID 2>/dev/null
        wait $SERVER_PID 2>/dev/null
        echo "✅ Server stopped"
    fi
}

# Function to restart server
restart_server() {
    echo "🔄 Restarting server due to file change..."
    stop_server
    sleep 1
    start_server
}

# Trap to clean up on exit
trap 'stop_server; echo "👋 Hot reloading server stopped"; exit 0' INT TERM

# Start initial server
start_server

# Watch for file changes and restart server
echo "👀 Watching for file changes..."

# Get absolute path to source directory
SRC_DIR="$(dirname "$0")/../../src"

# Check if fswatch is available (macOS preferred)
if command -v fswatch >/dev/null 2>&1; then
    echo "🍎 Using fswatch (macOS native file watcher)"
    echo "📁 Watching: $SRC_DIR"
    fswatch -o "$SRC_DIR" | while read num; do
        echo "📝 File change detected!"
        restart_server
        sleep 2  # Debounce rapid changes
    done
else
    echo "⏰ Using polling method (no fswatch available)"
    # Fallback: Simple polling approach
    LAST_MOD=$(find "$SRC_DIR" -type f -name "*.hs" -exec stat -f "%m" {} \; | sort -n | tail -1)
    while true; do
        sleep 2
        CURRENT_MOD=$(find "$SRC_DIR" -type f -name "*.hs" -exec stat -f "%m" {} \; | sort -n | tail -1)
        if [ "$CURRENT_MOD" != "$LAST_MOD" ]; then
            echo "📝 File change detected!"
            restart_server
            LAST_MOD=$CURRENT_MOD
            sleep 2  # Debounce
        fi
    done
fi
