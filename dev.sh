#!/bin/bash

# PSYOP Development Script
# Usage: ./dev.sh [start|stop|restart|status|clean]

case "$1" in
    start)
        echo "Starting PSYOP development server..."
        if pgrep -f "psyop-website-exe" > /dev/null; then
            echo "Server already running!"
        else
            stack run &
            echo "Server started on http://localhost:8080"
        fi
        ;;
    stop)
        echo "Stopping PSYOP development server..."
        pkill -f "psyop-website-exe"
        echo "Server stopped"
        ;;
    restart)
        echo "Restarting PSYOP development server..."
        ./dev.sh stop
        sleep 2
        ./dev.sh start
        ;;
    status)
        if pgrep -f "psyop-website-exe" > /dev/null; then
            echo "Server is running"
            ps aux | grep "psyop-website-exe" | grep -v grep
        else
            echo "Server is not running"
        fi
        ;;
    clean)
        echo "Cleaning up processes..."
        pkill -f "psyop-website-exe"
        pkill -f "stack"
        echo "Cleanup complete"
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|status|clean}"
        echo ""
        echo "Commands:"
        echo "  start   - Start the development server"
        echo "  stop    - Stop the development server"
        echo "  restart - Restart the development server"
        echo "  status  - Check server status"
        echo "  clean   - Clean up all related processes"
        exit 1
        ;;
esac
