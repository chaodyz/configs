#!/bin/bash

# Keep MS Teams active by moving the mouse slightly every 4 minutes
# Requires `cliclick`: install via `brew install cliclick`

# Check if cliclick is installed
if ! command -v cliclick &> /dev/null; then
    echo "⚠️  'cliclick' is not installed. Installing via Homebrew..."
    brew install cliclick
fi

echo "✅ Teams anti-away script started. Press Ctrl+C to stop."

while true; do
    # Get current mouse position
    POS=$(cliclick p | grep -oE '[0-9]+,[0-9]+')
    X=$(echo "$POS" | cut -d',' -f1)
    Y=$(echo "$POS" | cut -d',' -f2)

    # Move the mouse slightly and back
    cliclick "m:$((X+1)),$((Y+1))"
    sleep 1
    cliclick "m:$X,$Y"

    # Wait 4 minutes before repeating
    sleep 240
done