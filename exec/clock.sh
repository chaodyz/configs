#!/bin/bash

# Define log file
LOG_FILE="$HOME/work_time_log.txt"

# Get current timestamp
TIMESTAMP=$(date "+%Y-%m-%d %H:%M:%S")

# Function to clock in
clock_in() {
    echo "Clocking in at $TIMESTAMP"
    echo "IN  | $TIMESTAMP" >> "$LOG_FILE"
}

# Function to clock out
clock_out() {
    echo "Clocking out at $TIMESTAMP"
    echo "OUT | $TIMESTAMP" >> "$LOG_FILE"
}

# Function to show log
show_log() {
    echo "----- Work Log -----"
    cat "$LOG_FILE"
}

# Menu for user input
echo "1) Clock In"
echo "2) Clock Out"
echo "3) Show Log"
echo "4) Exit"
read -p "Select an option: " choice

case $choice in
    1) clock_in ;;
    2) clock_out ;;
    3) show_log ;;
    4) exit 0 ;;
    *) echo "Invalid option. Try again." ;;
esac
