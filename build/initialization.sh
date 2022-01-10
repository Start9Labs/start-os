#!/bin/bash

# Update repositories, install dependencies, do some initial configurations, set hostname, enable embassy-init, and config Tor
set -e

! test -f /etc/docker/daemon.json || rm /etc/docker/daemon.json
mount -o remount,rw /boot/firmware

apt-get update
apt-get purge -y \
	bluez \
	unattended-upgrades
apt-get install -y \
	docker.io \
	tor \
	nginx \
	libavahi-client3 \
	avahi-daemon \
	avahi-utils \
	iotop \
	bmon \
	exfat-utils \
	sqlite3 \
	wireless-tools \
	net-tools \
	ecryptfs-utils \
	cifs-utils \
	samba-common-bin \
	ntp \
	network-manager
apt-get autoremove -y
apt-get upgrade -y

sed -i 's/Restart=on-failure/Restart=always/g' /lib/systemd/system/tor@default.service
sed -i '/}/i \ \ \ \ application\/wasm \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ wasm;' /etc/nginx/mime.types
sed -i 's/# server_names_hash_bucket_size 64;/server_names_hash_bucket_size 128;/g' /etc/nginx/nginx.conf
sed -i 's/ExecStart=\/sbin\/wpa_supplicant -u -s -O \/run\/wpa_supplicant/ExecStart=\/sbin\/wpa_supplicant -u -s -O \/run\/wpa_supplicant -c \/etc\/wpa_supplicant.conf -i wlan0/g' /lib/systemd/system/wpa_supplicant.service
sed -i 's/#allow-interfaces=eth0/allow-interfaces=eth0,wlan0/g' /etc/avahi/avahi-daemon.conf
echo "auto wlan0" > /etc/network/interfaces
echo "iface wlan0 inet dhcp" >> /etc/network/interfaces
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

passwd -l ubuntu
echo 'overlayroot="tmpfs":swap=1,recurse=0' > /etc/overlayroot.local.conf
systemctl disable initialization.service
sudo systemctl restart NetworkManager
sync
reboot
