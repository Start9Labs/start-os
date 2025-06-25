#!/bin/bash
#
# StartOS WireGuard VPS Setup Tool
# https://github.com/start9labs/wg-vps-setup
# Derived from github.com/Nyr/wireguard-install (MIT License)

set -e

# Detect OS
# $os_version variables aren't always in use, but are kept here for convenience
if grep -qs "ubuntu" /etc/os-release; then
  os="ubuntu"
  os_version=$(grep 'VERSION_ID' /etc/os-release | cut -d '"' -f 2 | tr -d '.')
elif [[ -e /etc/debian_version ]]; then
  os="debian"
  os_version=$(grep -oE '[0-9]+' /etc/debian_version | head -1)
else
  >&2 echo "This installer seems to be running on an unsupported distribution.
Supported distros are Ubuntu, Debian."
  exit 1
fi

if [[ "$os" == "ubuntu" && "$os_version" -lt 2204 ]]; then
  >&2 echo "Ubuntu 22.04 or higher is required to use this installer.
This version of Ubuntu is too old and unsupported."
  exit 1
fi

if [[ "$os" == "debian" ]]; then
  if grep -q '/sid' /etc/debian_version; then
    >&2 echo "Debian Testing and Debian Unstable are unsupported by this installer."
    exit 1
  fi
  if [[ "$os_version" -lt 11 ]]; then
    >&2 echo "Debian 11 or higher is required to use this installer.
This version of Debian is too old and unsupported."
    exit 1
  fi
fi

# Detect environments where $PATH does not include the sbin directories
if ! grep -q sbin <<< "$PATH"; then
  >&2 echo '$PATH does not include sbin. Try using "su -" instead of "su".'
  exit 1
fi

# Detect if BoringTun (userspace WireGuard) needs to be used
if ! systemd-detect-virt -cq; then
  # Not running inside a container
  use_boringtun="0"
elif grep -q '^wireguard ' /proc/modules; then
  # Running inside a container, but the wireguard kernel module is available
  use_boringtun="0"
else
  # Running inside a container and the wireguard kernel module is not available
  use_boringtun="1"
fi

if [[ "$EUID" -ne 0 ]]; then
  >&2 echo "This installer needs to be run with superuser privileges."
  exit 1
fi

if [[ "$use_boringtun" -eq 1 ]]; then
  if [ "$(uname -m)" != "x86_64" ]; then
    >&2 echo "In containerized systems without the wireguard kernel module, this installer
supports only the x86_64 architecture.
The system runs on $(uname -m) and is unsupported."
    exit 1
  fi
  # TUN device is required to use BoringTun
  if [[ ! -e /dev/net/tun ]] || ! ( exec 7<>/dev/net/tun ) 2>/dev/null; then
    >&2 echo "The system does not have the TUN device available.
TUN needs to be enabled before running this installer."
    exit 1
  fi
fi

if ! which curl > /dev/null; then
  >&2 echo "curl is required to use this installer."
  >&2 apt-get update
  >&2 apt-get install -y curl
fi

if [ -z "$IP" ]; then
  >&2 echo "IP environment variable required"
  exit 1
fi
PRIMARY_INTERFACE=$(ip -o -4 addr show | awk -v ip="$IP" '$4 ~ "^"ip"/" {print $2; exit}')
  
# Use default port 51820
port="51820"

# Use STARTOS_HOSTNAME if set, otherwise default to "vps-clearnet"
client="${STARTOS_HOSTNAME:-vps-clearnet}"
# Sanitize the client name (although it should already be safe if it comes from STARTOS_HOSTNAME)
client=$(sed 's/[^0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-]/_/g' <<< "$client" | cut -c-15)



# Set up automatic updates for BoringTun if the user is fine with that
# if [[ "$use_boringtun" -eq 1 ]]; then
#   >&2 echo
#   >&2 echo "BoringTun will be installed to set up WireGuard in the system."
#   read -p "Should automatic updates be enabled for it? [Y/n]: " boringtun_updates
#   until [[ "$boringtun_updates" =~ ^[yYnN]*$ ]]; do
#     echo "$remove: invalid selection."
#     read -p "Should automatic updates be enabled for it? [Y/n]: " boringtun_updates
#   done
#   [[ -z "$boringtun_updates" ]] && boringtun_updates="y"
#   if [[ "$boringtun_updates" =~ ^[yY]$ ]]; then
#     cron="cron"
#   fi
# fi

# Install iptables if iptables are not already available
if ! which iptables > /dev/null; then
  # iptables is way less invasive than firewalld so no warning is given
  firewall="iptables"
fi
# Install WireGuard
# If BoringTun is not required, set up with the WireGuard kernel module
if [[ "$use_boringtun" -eq 0 ]]; then
  if [[ "$os" == "ubuntu" ]]; then
    # Ubuntu
    >&2 apt-get update
    >&2 apt-get install -y wireguard $firewall
  elif [[ "$os" == "debian" ]]; then
    # Debian
    >&2 apt-get update
    >&2 apt-get install -y wireguard $firewall
  fi
else
  # Install required packages
  if [[ "$os" == "ubuntu" ]]; then
    # Ubuntu
    >&2 apt-get update
    >&2 apt-get install -y ca-certificates $cron $firewall
    >&2 apt-get install -y wireguard-tools --no-install-recommends
  elif [[ "$os" == "debian" ]]; then
    # Debian
    >&2 apt-get update
    >&2 apt-get install -y ca-certificates $cron $firewall
    >&2 apt-get install -y wireguard-tools --no-install-recommends
  fi
  # Grab the BoringTun binary curl and extract into the right place.
  # Don't use this service elsewhere without permission! Contact me before you do!
  curl -sL https://wg.nyr.be/1/latest/download | tar xz -C /usr/local/sbin/ --wildcards 'boringtun-*/boringtun' --strip-components 1
  # Configure wg-quick to use BoringTun
  mkdir -p /etc/systemd/system/wg-quick@wg0.service.d/
  echo "[Service]
Environment=WG_QUICK_USERSPACE_IMPLEMENTATION=boringtun
Environment=WG_SUDO=1" > /etc/systemd/system/wg-quick@wg0.service.d/boringtun.conf
fi

privkey=$(wg genkey)
pubkey=$(echo $privkey | wg pubkey)

WG_SUBNET=${WG_SUBNET:-10.59.0.0}
WG_GATEWAY="${WG_SUBNET%.0}.1"
WG_FIRSTPEER="${WG_SUBNET%.0}.2"

cat << EOF > /etc/wireguard/wg0.conf
[Interface]
Address = ${WG_GATEWAY}/24
PrivateKey = $privkey
ListenPort = $port
PostUp = wg addconf wg0 /etc/wireguard/peers.conf
EOF
chmod 600 /etc/wireguard/wg0.conf

firstpeer_privkey=$(wg genkey)
firstpeer_pubkey=$(echo $firstpeer_privkey | wg pubkey)

cat << EOF > /etc/wireguard/peers.conf
[Peer]
Endpoint = $IP:$port
PublicKey = $pubkey
AllowedIPs = ${WG_GATEWAY}/32
PersistentKeepalive = 25

[Peer]
PublicKey = ${firstpeer_pubkey}
AllowedIPs = ${WG_FIRSTPEER}/32
PersistentKeepalive = 25

EOF
chmod 600 /etc/wireguard/peers.conf

systemctl enable --now wg-quick@wg0.service

# Enable net.ipv4.ip_forward for the system
echo 'net.ipv4.ip_forward=1' > /etc/sysctl.d/99-wireguard-forward.conf
echo 1 > /proc/sys/net/ipv4/ip_forward

iptables_path=$(command -v iptables)
ip6tables_path=$(command -v ip6tables)
# nf_tables is not available as standard in OVZ kernels. So use iptables-legacy
# if we are in OVZ, with a nf_tables backend and iptables-legacy is available.
if [[ $(systemd-detect-virt) == "openvz" ]] && readlink -f "$(command -v iptables)" | grep -q "nft" && hash iptables-legacy 2>/dev/null; then
  iptables_path=$(command -v iptables-legacy)
  ip6tables_path=$(command -v ip6tables-legacy)
fi


cat << EOF > /etc/systemd/system/wg-iptables.service
[Unit]
Before=network.target

[Service]
Type=oneshot
RemainAfterExit=yes
# IPv4 rules
ExecStart=$iptables_path -t nat -A POSTROUTING -s ${WG_SUBNET}/24 ! -d ${WG_SUBNET}/24 -j SNAT --to $ip
ExecStart=$iptables_path -I INPUT -p udp --dport $port -j ACCEPT
ExecStart=$iptables_path -I FORWARD -s ${WG_SUBNET}/24 -j ACCEPT
ExecStart=$iptables_path -I FORWARD -m state --state RELATED,ESTABLISHED -j ACCEPT
ExecStart=$iptables_path -t nat -A POSTROUTING -o $PRIMARY_INTERFACE -j MASQUERADE
ExecStart=$iptables_path -t nat -A PREROUTING -i $PRIMARY_INTERFACE -p tcp ! --dport 22 -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStart=$iptables_path -t nat -A PREROUTING -i $PRIMARY_INTERFACE -p udp -m multiport ! --dports 22,$port -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStart=$iptables_path -t nat -A PREROUTING -i wg0 -s ${WG_SUBNET}/24 -d $ip -p tcp ! --dport 22 -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStart=$iptables_path -t nat -A PREROUTING -i wg0 -s ${WG_SUBNET}/24 -d $ip -p udp -m multiport ! --dports 22,$port -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStart=$iptables_path -t nat -A POSTROUTING -o wg0 -s ${WG_SUBNET}/24 -d ${WG_FIRSTPEER}/32 -p tcp ! --dport 22 -j SNAT --to-source ${WG_GATEWAY}
ExecStart=$iptables_path -t nat -A POSTROUTING -o wg0 -s ${WG_SUBNET}/24 -d ${WG_FIRSTPEER}/32 -p udp -m multiport ! --dports 22,$port -j SNAT --to-source ${WG_GATEWAY}
ExecStart=$iptables_path -A FORWARD -j ACCEPT
# IPv4 stop rules
ExecStop=$iptables_path -t nat -D POSTROUTING -s ${WG_SUBNET}/24 ! -d ${WG_SUBNET}/24 -j SNAT --to $ip
ExecStop=$iptables_path -D INPUT -p udp --dport $port -j ACCEPT
ExecStop=$iptables_path -D FORWARD -s ${WG_SUBNET}/24 -j ACCEPT
ExecStop=$iptables_path -D FORWARD -m state --state RELATED,ESTABLISHED -j ACCEPT
ExecStop=$iptables_path -t nat -D POSTROUTING -o $PRIMARY_INTERFACE -j MASQUERADE
ExecStop=$iptables_path -t nat -D PREROUTING -i $PRIMARY_INTERFACE -p tcp ! --dport 22 -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStop=$iptables_path -t nat -D PREROUTING -i $PRIMARY_INTERFACE -p udp -m multiport ! --dports 22,$port -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStop=$iptables_path -t nat -D PREROUTING -i wg0 -s ${WG_SUBNET}/24 -d $ip -p tcp ! --dport 22 -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStop=$iptables_path -t nat -D PREROUTING -i wg0 -s ${WG_SUBNET}/24 -d $ip -p udp -m multiport ! --dports 22,$port -j DNAT --to-destination ${WG_FIRSTPEER}
ExecStop=$iptables_path -t nat -D POSTROUTING -o wg0 -s ${WG_SUBNET}/24 -d ${WG_FIRSTPEER}/32 -p tcp ! --dport 22 -j SNAT --to-source ${WG_GATEWAY}
ExecStop=$iptables_path -t nat -D POSTROUTING -o wg0 -s ${WG_SUBNET}/24 -d ${WG_FIRSTPEER}/32 -p udp -m multiport ! --dports 22,$port -j SNAT --to-source ${WG_GATEWAY}
ExecStop=$iptables_path -D FORWARD -j ACCEPT

[Install]
WantedBy=multi-user.target
EOF

cat << EOF
[Interface]
Address = ${WG_FIRSTPEER}/24
PrivateKey = $firstpeer_privkey

EOF
cat /etc/wireguard/peers.conf

