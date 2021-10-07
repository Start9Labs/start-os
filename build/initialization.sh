#!/bin/bash

# Update repositories, install dependencies, do some initial configurations, set hostname, enable embassy-init, and config Tor
set -e
apt update
apt install -y \
	docker.io \
	tor \
	nginx \
	libavahi-client3 \
	avahi-daemon \
	iotop \
	bmon \
	zfsutils-linux \
	exfat-utils \
	sqlite3
sed -i 's/"1"/"0"/g' /etc/apt/apt.conf.d/20auto-upgrades
sed -i 's/Restart=on-failure/Restart=always/g' /lib/systemd/system/tor@default.service
sed -i '/}/i \ \ \ \ application\/wasm \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ wasm;' /etc/nginx/mime.types
sed -i 's/# server_names_hash_bucket_size 64;/server_names_hash_bucket_size 128;/g' /etc/nginx/nginx.conf
sed -i 's/ExecStart=\/sbin\/wpa_supplicant -u -s -O \/run\/wpa_supplicant/ExecStart=\/sbin\/wpa_supplicant -u -s -O \/run\/wpa_supplicant -c \/etc\/wpa_supplicant.conf -i wlan0/g' /lib/systemd/system/wpa_supplicant.service
mkdir -p /etc/nginx/ssl
docker run --privileged --rm tonistiigi/binfmt --install all
docker network create -d bridge --subnet 172.18.0.1/16 start9 || true
echo '{ "storage-driver": "zfs" }' > /etc/docker/daemon.json
mkdir -p /etc/embassy
hostnamectl set-hostname "embassy"
systemctl enable embassyd.service embassy-init.service
echo 'overlayroot="tmpfs"' > /etc/overlayroot.local.conf
cat << EOF > /etc/tor/torrc
SocksPort 0.0.0.0:9050
SocksPolicy accept 127.0.0.1
SocksPolicy accept 172.18.0.0/16
SocksPolicy reject *
ControlPort 9051
CookieAuthentication 1
EOF

systemctl disable initialization.service
sync
reboot
