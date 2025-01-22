#!/bin/bash

# Colors for better output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[1;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# --- Constants ---
readonly WIREGUARD_INSTALL_URL="https://raw.githubusercontent.com/k0gen/wireguard-install/master/wireguard-install.sh"
readonly SSH_KEY_DIR="/home/start9/.ssh"
readonly SSH_KEY_NAME="id_ed25519"
readonly SSH_PRIVATE_KEY="$SSH_KEY_DIR/$SSH_KEY_NAME"
readonly SSH_PUBLIC_KEY="$SSH_PRIVATE_KEY.pub"

# Store original arguments
SCRIPT_ARGS=("$@")

# --- Functions ---

# Function to ensure script runs with root privileges by auto-elevating if needed
check_root() {
    if [[ "$EUID" -ne 0 ]]; then
        exec sudo "$0" "${SCRIPT_ARGS[@]}"
    fi
    sudo chown -R start9:start9 "$SSH_KEY_DIR"
}

# Function to print banner
print_banner() {
    echo -e "${BLUE}"
    echo "================================================"
    echo -e "     ${NC}StartOS WireGuard VPS Setup Tool${BLUE}              "
    echo "================================================"
    echo -e "${NC}"
}

# Function to print usage
print_usage() {
    echo -e "Usage: $0 [-h] [-i IP] [-u USERNAME] [-p PORT] [-k SSH_KEY]"
    echo "Options:"
    echo "  -h    Show this help message"
    echo "  -i    VPS IP address"
    echo "  -u    SSH username (default: root)"
    echo "  -p    SSH port (default: 22)"
    echo "  -k    Path to the custom SSH private key (optional)"
    echo "        If no key is provided, the default key '$SSH_PRIVATE_KEY' will be used."
}

# Function to display end message
display_end_message() {
    echo -e "\n${BLUE}------------------------------------------------------------------${NC}"
    echo -e "${NC}WireGuard server setup complete!"
    echo -e "${BLUE}------------------------------------------------------------------${NC}"
    echo -e "\n${YELLOW}To expose your services to the Clearnet, use the following commands on your StartOS system (replace placeholders):${NC}"
    echo -e "\n   ${YELLOW}1. Initialize ACME (This only needs to be done once):${NC}"
    echo "      start-cli net acme init --provider=letsencrypt --contact=mailto:your-email@example.com"
    echo -e "\n   ${YELLOW}2. Expose 'hello-world' on port 80 through VPS:${NC}"
    echo "      start-cli package host hello-world binding ui-multi set-public 80"
    echo -e "\n   ${YELLOW}3. Add a domain to your 'hello-world' service:${NC}"
    echo "      start-cli package host hello-world address ui-multi domain add your-domain.example.com --acme=letsencrypt"
    echo -e "\n   ${YELLOW}Replace '${NC}your-email@example.com${YELLOW}' with your actual email address, '${NC}your-domain.example.com${YELLOW}' with your actual domain and '${NC}hello-world${YELLOW}' with your actual service id.${NC}"
    echo -e "${BLUE}------------------------------------------------------------------${NC}"
}

# Function to validate IP address
validate_ip() {
    local ip=$1
    if [[ $ip =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]; then
        return 0
    else
        return 1
    fi
}

# Function to handle StartOS connection (download only)
handle_startos_connection() {
    echo -e "${BLUE}Fetching the WireGuard configuration file...${NC}"

    # Fetch the client configuration file
    config_file=$(ssh -i "$SSH_PRIVATE_KEY" -o StrictHostKeyChecking=no -p "$SSH_PORT" "$SSH_USER@$VPS_IP" 'ls -t ~/*.conf 2>/dev/null | head -n 1')
    if [ -z "$config_file" ]; then
        echo -e "${RED}Error: No WireGuard configuration file found on the remote server.${NC}"
        return 1 # Exit with error
    fi
    CONFIG_NAME=$(basename "$config_file")

    # Download the configuration file
    if ! scp -i "$SSH_PRIVATE_KEY" -o StrictHostKeyChecking=no -P "$SSH_PORT" "$SSH_USER@$VPS_IP":~/"$CONFIG_NAME" ./; then
        echo -e "${RED}Error: Failed to download the WireGuard configuration file.${NC}"
        return 1 # Exit with error
    fi
    echo -e "${GREEN}WireGuard configuration file '$CONFIG_NAME' downloaded successfully.${NC}"
    return 0
}

# Function to import WireGuard configuration
import_wireguard_config() {
    local config_name="$1"
    if [ -z "$config_name" ]; then
        echo -e "${RED}Error: Configuration file name is missing.${NC}"
        return 1
    fi

    local connection_name=$(basename "$config_name" .conf) #Extract base name without extension

    # Check if the connection with same name already exists
    if nmcli connection show --active | grep -q "^${connection_name}\s"; then
        read -r -p "A connection with the name '$connection_name' already exists. Do you want to override it? (y/N): " answer
        if [[ "$answer" =~ ^[Yy]$ ]]; then
            nmcli connection delete "$connection_name"
            if [ $? -ne 0 ]; then
                echo -e "${RED}Error: Failed to delete existing connection '$connection_name'.${NC}"
                return 1
            fi
            # Import if user chose to override or if connection did not exist
            if ! nmcli connection import type wireguard file "$config_name"; then
                echo -e "${RED}Error: Failed to import the WireGuard configuration using NetworkManager.${NC}"
                rm -f "$config_name"
                return 1
            fi
            echo -e "${GREEN}WireGuard configuration '$config_name' has been imported to NetworkManager.${NC}"
            rm -f "$config_name"
            display_end_message
        else
            echo -e "${BLUE}Skipping import of the WireGuard configuration.${NC}"
            rm -f "$config_name"
            return 0
        fi
    else
        # Import if connection did not exist
        if command -v nmcli &>/dev/null; then
            if ! nmcli connection import type wireguard file "$config_name"; then
                echo -e "${RED}Error: Failed to import the WireGuard configuration using NetworkManager.${NC}"
                rm -f "$config_name"
                return 1
            fi
            echo -e "${GREEN}WireGuard configuration '$config_name' has been imported to NetworkManager.${NC}"
            rm -f "$config_name"
            display_end_message
        else
            echo -e "${YELLOW}Warning: NetworkManager 'nmcli' not found. Configuration file '$config_name' saved in current directory.${NC}"
            echo -e "${YELLOW}Import the configuration to your StartOS manually by going to NetworkManager or using wg-quick up <config> command${NC}"
        fi
    fi
    return 0
}

# Function to download the install script
download_install_script() {
    echo -e "${BLUE}Downloading latest WireGuard install script...${NC}"
    # Download the script
    if ! curl -sSf "$WIREGUARD_INSTALL_URL" -o wireguard-install.sh; then
        echo -e "${RED}Failed to download WireGuard installation script.${NC}"
        return 1
    fi
    chmod +x wireguard-install.sh
    if [ $? -ne 0 ]; then
        echo -e "${RED}Failed to chmod +x wireguard install script.${NC}"
        return 1
    fi
    echo -e "${GREEN}WireGuard install script downloaded successfully!${NC}"
    return 0
}

# Function to install WireGuard
install_wireguard() {
    echo -e "\n${BLUE}Installing WireGuard...${NC}"

    # Check if install script exist
    if [ ! -f "wireguard-install.sh" ]; then
        echo -e "${RED}WireGuard install script is missing. Did it failed to download?${NC}"
        return 1
    fi

    # Run the remote install script and let it complete
    if ! ssh -o ConnectTimeout=60 -i "$SSH_PRIVATE_KEY" -o StrictHostKeyChecking=no -p "$SSH_PORT" -t "$SSH_USER@$VPS_IP" "bash -c 'export TERM=xterm-256color; export STARTOS_HOSTNAME=$(hostname); bash ~/wireguard-install.sh'"; then
        echo -e "${RED}WireGuard installation failed on remote server.${NC}"
        return 1
    fi

    # Test if wireguard installed
    if ! ssh -q -o BatchMode=yes -o ConnectTimeout=5 -i "$SSH_PRIVATE_KEY" -o StrictHostKeyChecking=no -p "$SSH_PORT" "$SSH_USER@$VPS_IP" "test -f /etc/wireguard/wg0.conf"; then
        echo -e "\n${RED}WireGuard installation failed because /etc/wireguard/wg0.conf is missing, which means the script removed it.${NC}"
        return 1
    fi

    echo -e "\n${GREEN}WireGuard installation completed successfully!${NC}"
    return 0
}

# --- Main Script ---
# Initialize variables
VPS_IP=""
SSH_USER="root"
SSH_PORT="22"
CUSTOM_SSH_KEY=""
CONFIG_NAME=""

# Check if the script is run as root before anything else
check_root

# Print banner
print_banner

# Parse command line arguments
while getopts "hi:u:p:k:" opt; do
    case $opt in
    h)
        print_usage
        exit 0
        ;;
    i)
        VPS_IP=$OPTARG
        ;;
    u)
        SSH_USER=$OPTARG
        ;;
    p)
        SSH_PORT=$OPTARG
        ;;
    k)
        CUSTOM_SSH_KEY=$OPTARG
        ;;
    \?)
        echo "Invalid option: -$OPTARG" >&2
        print_usage
        exit 1
        ;;
    esac
done

# Check if custom SSH key is passed and update the private key variable
if [ -n "$CUSTOM_SSH_KEY" ]; then
    if [ ! -f "$CUSTOM_SSH_KEY" ]; then
        echo -e "${RED}Custom SSH key '$CUSTOM_SSH_KEY' not found.${NC}"
        exit 1
    fi
    SSH_PRIVATE_KEY="$CUSTOM_SSH_KEY"
    SSH_PUBLIC_KEY="$CUSTOM_SSH_KEY.pub"
else
    # Use default StartOS SSH key
    if [ ! -f "$SSH_PRIVATE_KEY" ]; then
        echo -e "${RED}No SSH key found at default location '$SSH_PRIVATE_KEY'. Please ensure StartOS SSH keys are properly configured.${NC}"
        exit 1
    fi
fi

if [ ! -f "$SSH_PUBLIC_KEY" ]; then
    echo -e "${RED}Public key '$SSH_PUBLIC_KEY' not found. Please ensure both private and public keys exist.${NC}"
    exit 1
fi

# If VPS_IP is not provided via command line, ask for it
if [ -z "$VPS_IP" ]; then
    while true; do
        echo -n "Please enter your VPS IP address: "
        read VPS_IP
        if validate_ip "$VPS_IP"; then
            break
        else
            echo -e "${RED}Invalid IP address format. Please try again.${NC}"
        fi
    done
fi

# Confirm SSH connection details
echo -e "\n${GREEN}Connection details:${NC}"
echo "VPS IP: $VPS_IP"
echo "SSH User: $SSH_USER"
echo "SSH Port: $SSH_PORT"

echo -e "\n${GREEN}Proceeding with SSH key-based authentication...${NC}\n"

# Copy SSH public key to the remote server
if ! ssh-copy-id -i "$SSH_PUBLIC_KEY" -o StrictHostKeyChecking=no -p "$SSH_PORT" "$SSH_USER@$VPS_IP" >/dev/null 2>&1; then
    echo -e "${RED}Failed to copy SSH key to the remote server. Please ensure you have correct credentials.${NC}"
    exit 1
fi

echo -e "${GREEN}SSH key-based authentication configured successfully!${NC}"

# Test SSH connection using key-based authentication
echo -e "\nTesting SSH connection with key-based authentication..."
if ! ssh -q -o BatchMode=yes -o ConnectTimeout=5 -i "$SSH_PRIVATE_KEY" -o StrictHostKeyChecking=no -p "$SSH_PORT" "$SSH_USER@$VPS_IP" exit; then
    echo -e "${RED}SSH connection with key-based authentication failed. Please check your configuration.${NC}"
    exit 1
fi

echo -e "${GREEN}SSH connection successful with key-based authentication!${NC}"

# Download the WireGuard install script locally
if ! download_install_script; then
    echo -e "${RED}Failed to download the latest install script. Exiting...${NC}"
    exit 1
fi

# Upload the install script to the remote server
if ! scp -i "$SSH_PRIVATE_KEY" -o StrictHostKeyChecking=no -P "$SSH_PORT" wireguard-install.sh "$SSH_USER@$VPS_IP":~/; then
    echo -e "${RED}Failed to upload WireGuard install script to the remote server.${NC}"
    exit 1
fi

# Install WireGuard on remote server using the downloaded script
if ! install_wireguard; then
    echo -e "${RED}WireGuard installation failed.${NC}"
    exit 1
fi

# Remove the local install script
rm wireguard-install.sh >/dev/null 2>&1

# Handle the StartOS config (download)
if ! handle_startos_connection; then
    echo -e "${RED}StartOS configuration download failed!${NC}"
    exit 1
fi

# Import the configuration
if ! import_wireguard_config "$CONFIG_NAME"; then
    echo -e "${RED}StartOS configuration import failed or skipped!${NC}"
fi
