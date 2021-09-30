#!/bin/bash

# Update repositories, install dependencies, do some initial configurations, set hostname, enable embassy-init, and config Tor
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
docker run --privileged --rm tonistiigi/binfmt --install all
docker network create -d bridge --subnet 172.18.0.1/16 start9
echo '{ "storage-driver": "zfs" }' > /etc/docker/daemon.json
mkdir /etc/embassy
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
