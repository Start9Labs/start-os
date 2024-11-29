#!/bin/bash

# Define the output file
OUTPUT_FILE="system_debug_info.txt"

# Check if the script is run as root, if not, restart with sudo
if [ "$(id -u)" -ne 0 ]; then
    exec sudo bash "$0" "$@"
fi

# Create or clear the output file and add a header
echo "===================================================================" > "$OUTPUT_FILE"
echo "                     StartOS System Debug Information              " >> "$OUTPUT_FILE"
echo "===================================================================" >> "$OUTPUT_FILE"
echo "Generated on: $(date)" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to run a command if it exists and append its output to the file with headers
run_command() {
    local CMD="$1"
    local DESC="$2"
    local CMD_NAME="${CMD%% *}"  # Extract the command name (first word)

    if command_exists "$CMD_NAME"; then
        echo "===================================================================" >> "$OUTPUT_FILE"
        echo "COMMAND: $CMD" >> "$OUTPUT_FILE"
        echo "DESCRIPTION: $DESC" >> "$OUTPUT_FILE"
        echo "===================================================================" >> "$OUTPUT_FILE"
        echo "" >> "$OUTPUT_FILE"
        eval "$CMD" >> "$OUTPUT_FILE" 2>&1
        echo "" >> "$OUTPUT_FILE"
    else
        echo "===================================================================" >> "$OUTPUT_FILE"
        echo "COMMAND: $CMD" >> "$OUTPUT_FILE"
        echo "DESCRIPTION: $DESC" >> "$OUTPUT_FILE"
        echo "===================================================================" >> "$OUTPUT_FILE"
        echo "SKIPPED: Command not found" >> "$OUTPUT_FILE"
        echo "" >> "$OUTPUT_FILE"
    fi
}

# Collecting basic system information
run_command "start-cli --version; start-cli git-info" "StartOS CLI version and Git information"
run_command "hostname" "Hostname of the system"
run_command "uname -a" "Kernel version and system architecture"

# Services Info
run_command "podman stats --no-stream" "All Running Services"

# Collecting CPU information
run_command "lscpu" "CPU architecture information"
run_command "cat /proc/cpuinfo" "Detailed CPU information"

# Collecting memory information
run_command "free -h" "Available and used memory"
run_command "cat /proc/meminfo" "Detailed memory information"

# Collecting storage information
run_command "lsblk" "List of block devices"
run_command "df -h" "Disk space usage"
run_command "fdisk -l" "Detailed disk partition information"

# Collecting network information
run_command "ip a" "Network interfaces and IP addresses"
run_command "ip route" "Routing table"
run_command "netstat -i" "Network interface statistics"

# Collecting RAID information (if applicable)
run_command "cat /proc/mdstat" "List of RAID devices (if applicable)"

# Collecting virtualization information
run_command "egrep -c '(vmx|svm)' /proc/cpuinfo" "Check if CPU supports virtualization"
run_command "systemd-detect-virt" "Check if the system is running inside a virtual machine"

# Final message
echo "===================================================================" >> "$OUTPUT_FILE"
echo "                    End of StartOS System Debug Information        " >> "$OUTPUT_FILE"
echo "===================================================================" >> "$OUTPUT_FILE"

# Prompt user to send the log file to a Start9 Technician
echo "System debug information has been collected in $OUTPUT_FILE."
echo ""
echo "Would you like to send this log file to a Start9 Technician? (yes/no)"
read SEND_LOG

if [[ "$SEND_LOG" == "yes" || "$SEND_LOG" == "y" ]]; then
    if command -v wormhole >/dev/null 2>&1; then
        echo ""
        echo "==================================================================="
        echo "      Running wormhole to send the file. Please follow the          "
        echo "      instructions and provide the code to the Start9 support team. "
        echo "==================================================================="
        wormhole send "$OUTPUT_FILE"
        echo "==================================================================="
    else
        echo "Error: wormhole command not found."
    fi
else
    echo "Log file not sent. You can manually share $OUTPUT_FILE with the Start9 support team if needed."
fi