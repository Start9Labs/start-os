#!/bin/bash
set -e

# introduce start9 username and embassy as default password
if ! awk -F: '{ print $1 }' /etc/passwd | grep start9
then
	usermod -l start9 -d /home/start9 -m pi
	groupmod --new-name start9 pi
	echo start9:embassy | chpasswd
fi

passwd -l start9

while ! ping -q -w 1 -c 1 `ip r | grep default | cut -d ' ' -f 3` > /dev/null; do
	>&2 echo "Waiting for internet connection..."
	sleep 1
done
echo "Connected to network"

# change timezone
timedatectl set-timezone Etc/UTC

! test -f /etc/docker/daemon.json || rm /etc/docker/daemon.json
mount -o remount,rw /boot

apt-mark hold raspberrypi-bootloader
apt-mark hold raspberrypi-kernel

# Convert all repos to use https:// before apt update
sed -i "s/http:/https:/g" /etc/apt/sources.list /etc/apt/sources.list.d/*.list

apt-get update
apt-get install -y \
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
	ncdu \
	postgresql \
	pgloader

# Setup repository from The Guardian Project and install latest stable Tor daemon
echo "deb     [arch=arm64 signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org bullseye main" >> /etc/apt/sources.list.d/tor.list
echo "deb-src [arch=arm64 signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org bullseye main" >> /etc/apt/sources.list.d/tor.list
wget -qO- https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null
apt update && apt install -y tor deb.torproject.org-keyring

curl -fsSL https://get.docker.com | sh # TODO: commit this script into git instead of live fetching it

# enable embassyd dns server
systemctl enable systemd-resolved
sed -i '/\(^\|#\)DNS=/c\DNS=127.0.0.1' /etc/systemd/resolved.conf
systemctl start systemd-resolved

apt-get remove --purge openresolv dhcpcd5 -y
systemctl disable wpa_supplicant.service

sudo -u postgres createuser root
sudo -u postgres createdb secrets -O root
systemctl disable postgresql.service

ln -rsf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf

systemctl disable bluetooth.service
systemctl disable hciuart.service
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

cat << EOF > /etc/NetworkManager/NetworkManager.conf
[main]
plugins=ifupdown,keyfile
dns=systemd-resolved

[ifupdown]
managed=false
EOF


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

# making that *sudo docker stats* command fulfil its purpose by displaying all metrics
sed -i 's/rootwait quiet.*/rootwait cgroup_enable=cpuset cgroup_memory=1 cgroup_enable=memory quiet/g' /boot/cmdline.txt

systemctl disable nc-broadcast.service
systemctl disable initialization.service
sudo systemctl restart NetworkManager

echo "fs.inotify.max_user_watches=1048576" > /etc/sysctl.d/97-embassy.conf

sync

reboot

