#!/bin/bash

# Update repositories, install dependencies, do some initial configurations, set hostname, enable embassy-init, and config Tor
set -e

# introduce start9 username and embassy as default password
if ! awk -F: '{ print $1}' /etc/passwd | grep start9
then
	usermod -l start9 -d /home/start9 -m pi
	groupmod --new-name start9 pi
	echo start9:embassy | chpasswd
fi

passwd -l start9

! test -f /etc/docker/daemon.json || rm /etc/docker/daemon.json
mount -o remount,rw /boot

apt-get update
apt-get install -y \
	tor \
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
	ntp \
	network-manager \
	vim \
	jq \
	ncdu

curl -fsSL https://get.docker.com | sh # TODO: commit this script into git instead of live fetching it

apt-get purge openresolv dhcpcd5 -y
systemctl disable wpa_supplicant.service 

apt-get autoremove -y
apt-get upgrade -y

sed -i 's/Restart=on-failure/Restart=always/g' /lib/systemd/system/tor@default.service
sed -i '/}/i \ \ \ \ application\/wasm \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ wasm;' /etc/nginx/mime.types
sed -i 's/# server_names_hash_bucket_size 64;/server_names_hash_bucket_size 128;/g' /etc/nginx/nginx.conf
# sed -i 's/ExecStart=\/sbin\/wpa_supplicant -u -s -O \/run\/wpa_supplicant/ExecStart=\/sbin\/wpa_supplicant -u -s -O \/run\/wpa_supplicant -c \/etc\/wpa_supplicant.conf -i wlan0/g' /lib/systemd/system/wpa_supplicant.service
sed -i 's/#allow-interfaces=eth0/allow-interfaces=eth0,wlan0/g' /etc/avahi/avahi-daemon.conf
echo "#" > /etc/network/interfaces
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

cat /embassy-os/product_key.txt | tr -d '\n' | sha256sum | head -c 32 | sed 's/$/\n/' > /etc/machine-id

raspi-config nonint enable_overlayfs

# create a copy of the cmdline *without* the quirk string, so that it can be easily amended
sudo cp /boot/cmdline.txt /boot/cmdline.txt.orig
sudo sed -i 's/^/usb-storage.quirks=152d:0562:u /g' /boot/cmdline.txt

systemctl disable initialization.service
sudo systemctl restart NetworkManager

sync

# TODO: clean out ssh host keys
reboot
