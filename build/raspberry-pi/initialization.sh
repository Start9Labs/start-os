#!/bin/bash
set -e

# introduce start9 username and embassy as default password
if ! awk -F: '{ print $1 }' /etc/passwd | grep start9
then
	usermod -l start9 -d /home/start9 -m pi
	groupmod --new-name start9 pi
	echo start9:embassy | chpasswd
fi

START=$(date +%s)
while ! ping -q -w 1 -c 1 `ip r | grep default | cut -d ' ' -f 3` > /dev/null; do
	>&2 echo "Waiting for internet connection..."
	sleep 1
	if [ "$[$START + 60]" -lt $(date +%s) ]; then
		>&2 echo "Timed out waiting for internet connection..."
		exit 1
	fi
done
echo "Connected to network"

# Convert all repos to use https:// before apt update
sed -i "s/http:/https:/g" /etc/apt/sources.list /etc/apt/sources.list.d/*.list

. /usr/lib/embassy/scripts/add-apt-sources

apt-get update
apt-get upgrade -y
apt-get install -y $(cat /usr/lib/embassy/depends)
apt-get remove --purge -y $(cat /usr/lib/embassy/conflicts) beep
apt-get autoremove -y

systemctl stop tor

. /usr/lib/embassy/scripts/postinst

systemctl enable embassyd.service embassy-init.service

. /usr/lib/embassy/scripts/enable-kiosk

sed -i 's/^/usb-storage.quirks=152d:0562:u,14cd:121c:u,0781:cfcb:u /g' /boot/cmdline.txt

# making that *sudo docker stats* command fulfil its purpose by displaying all metrics
sed -i 's/rootwait quiet.*/rootwait cgroup_enable=cpuset cgroup_memory=1 cgroup_enable=memory quiet/g' /boot/cmdline.txt

systemctl disable nc-broadcast.service
systemctl disable initialization.service

update-initramfs -c -k "$(uname -r)"

sed -i /boot/config.txt -e "/initramfs.*/d" 
echo initramfs "initrd.img-$(uname -r)" >> /boot/config.txt

sed -i /boot/cmdline.txt -e "s/^/boot=embassy /"

passwd -l start9

sync

reboot

