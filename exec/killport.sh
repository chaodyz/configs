#!/bin/bash

# Main script
read -p "Enter a port number: " port_number

# Check if the input is a valid number
if ! [[ "$port_number" =~ ^[0-9]+$ ]]; then
    echo "Invalid port number. Please enter a valid number."
    exit 1
fi

# Find all processes using the specified port
pids=($(lsof -ti :"$port_number" 2>/dev/null))

if [ ${#pids[@]} -eq 0 ]; then
    echo "No process found running on port $port_number."
    exit 1
fi

echo "Processes found on port $port_number:"
echo "---------------------------------"
echo "| PID       | Process Name     |"
echo "---------------------------------"

for pid in "${pids[@]}"; do
    process_name=$(ps -p "$pid" -o comm= 2>/dev/null)
    echo "| $pid  | $process_name  |"
done

echo "---------------------------------"

read -p "Enter the PID of the process you want to kill: " chosen_pid

# Check if the chosen PID is in the list
if [[ " ${pids[@]} " =~ " $chosen_pid " ]]; then
    read -p "Do you want to kill the process with PID $chosen_pid? (y/n): " confirm
    if [ "$confirm" = "y" ] || [ "$confirm" = "Y" ]; then
        # Attempt to kill the chosen process using sudo
        sudo kill -9 "$chosen_pid"
        
        # Check the exit status of the kill command
        if [ $? -eq 0 ]; then
            echo "Process with PID $chosen_pid has been killed."
        else
            echo "Failed to kill process with PID $chosen_pid."
        fi
    else
        echo "Process with PID $chosen_pid has not been killed."
    fi
else
    echo "Invalid PID. No matching process found."
fi
