#!/bin/bash

# Update repositories, install dependencies, do some initial configurations, set hostname, enable embassy-init, and config Tor
set -e

# introduce start9 username and embassy as default password
if ! awk -F: '{ print $1 }' /etc/passwd | grep start9
then
	usermod -l start9 -d /home/start9 -m pi
	groupmod --new-name start9 pi
	echo start9:embassy | chpasswd
fi

passwd -l start9

! test -f /etc/docker/daemon.json || rm /etc/docker/daemon.json
mount -o remount,rw /boot

apt-mark hold raspberrypi-bootloader
apt-mark hold raspberrypi-kernel

apt-get update
apt-get install -y \
	apt-transport-https \
	nginx \
	libavahi-client3 \
	avahi-daemon \
	avahi-utils \
	iotop \
	bmon \
	lvm2 \
	cryptsetup \
	exfat-utils \
	sqlite3 \
	wireless-tools \
	net-tools \
	ecryptfs-utils \
	cifs-utils \
	samba-common-bin \
	network-manager \
	vim \
	jq \
	ncdu

# Setup repository from The Guardian Project and install latest stable Tor daemon
touch /etc/apt/sources.list.d/tor.list
echo "deb     [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org bullseye main" >> /etc/apt/sources.list.d/tor.list
echo "deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org bullseye main" >> /etc/apt/sources.list.d/tor.list
wget -qO- https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null
apt update && apt install -y tor deb.torproject.org-keyring

curl -fsSL https://get.docker.com | sh # TODO: commit this script into git instead of live fetching it

apt-get purge openresolv dhcpcd5 -y
systemctl disable wpa_supplicant.service

systemctl disable bluetooth.service
systemctl disable triggerhappy.service

apt-get autoremove -y
apt-get upgrade -y

sed -i 's/Restart=on-failure/Restart=always/g' /lib/systemd/system/tor@default.service
sed -i 's/ExecStart=\/usr\/bin\/dockerd/ExecStart=\/usr\/bin\/dockerd --exec-opt native.cgroupdriver=systemd/g' /lib/systemd/system/docker.service
sed -i '/}/i \ \ \ \ application\/wasm \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ wasm;' /etc/nginx/mime.types
sed -i 's/# server_names_hash_bucket_size 64;/server_names_hash_bucket_size 128;/g' /etc/nginx/nginx.conf
sed -i 's/#allow-interfaces=eth0/allow-interfaces=eth0,wlan0/g' /etc/avahi/avahi-daemon.conf
echo "#" > /etc/network/interfaces
echo '{ "cgroup-parent": "docker-engine.slice" }' > /etc/docker/daemon.json
mkdir -p /etc/nginx/ssl

# fix to suppress docker warning, fixed in 21.xx release of docker cli: https://github.com/docker/cli/pull/2934
mkdir -p /root/.docker
touch /root/.docker/config.json

docker run --privileged --rm tonistiigi/binfmt --install all
docker network create -d bridge --subnet 172.18.0.1/16 start9 || true
mkdir -p /etc/embassy
hostnamectl set-hostname "embassy"
systemctl enable embassyd.service embassy-init.service
cat << EOF > /etc/tor/torrc
SocksPort 0.0.0.0:9050
SocksPolicy accept 127.0.0.1
SocksPolicy accept 172.18.0.0/16
SocksPolicy reject *
ControlPort 9051
CookieAuthentication 1
EOF

# enable embassyd dns server
systemctl enable systemd-resolved
sed -i '/\(^\|#\)DNS=/c\DNS=127.0.0.1' /etc/systemd/resolved.conf
sed -i '/prepend domain-name-servers/c\prepend domain-name-servers 127.0.0.53;' /etc/dhcp/dhclient.conf

if [ -f /embassy-os/product_key.txt ]
then
	cat /embassy-os/product_key.txt | tr -d '\n' | sha256sum | head -c 32 | sed 's/$/\n/' > /etc/machine-id
else
	head -c 16 /dev/urandom | xxd -p | sed 's/$/\n/' > /etc/machine-id
fi

systemctl stop tor
rm -rf /var/lib/tor/*

raspi-config nonint enable_overlayfs

# create a copy of the cmdline *without* the quirk string, so that it can be easily amended
sed -i 's/usb-storage.quirks=152d:0562:u,14cd:121c:u,0781:cfcb:u //g' /boot/cmdline.txt
cp /boot/cmdline.txt /boot/cmdline.txt.orig
sed -i 's/^/usb-storage.quirks=152d:0562:u,14cd:121c:u,0781:cfcb:u /g' /boot/cmdline.txt

systemctl disable initialization.service
sudo systemctl restart NetworkManager

sync

# TODO: clean out ssh host keys
reboot
