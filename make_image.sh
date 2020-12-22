#!/bin/bash

mv buster.img embassy.img
product_key=$(cat product_key)
loopdev=$(losetup -f -P embassy.img --show)
root_mountpoint="/mnt/start9-${product_key}-root"
boot_mountpoint="/mnt/start9-${product_key}-boot"
mkdir -p "${root_mountpoint}"
mkdir -p "${boot_mountpoint}"
mount "${loopdev}p2" "${root_mountpoint}"
mount "${loopdev}p1" "${boot_mountpoint}"
echo "${product_key}" > "${root_mountpoint}/root/agent/product_key"
echo -n "start9-" > "${root_mountpoint}/etc/hostname"
echo -n "${product_key}" | shasum -t -a 256 | cut -c1-8 >> "${root_mountpoint}/etc/hostname"
cat "${root_mountpoint}/etc/hosts" | grep -v "127.0.1.1" > "${root_mountpoint}/etc/hosts.tmp"
echo -ne "127.0.1.1\tstart9-" >> "${root_mountpoint}/etc/hosts.tmp"
echo -n "${product_key}" | shasum -t -a 256 | cut -c1-8 >> "${root_mountpoint}/etc/hosts.tmp"
mv "${root_mountpoint}/etc/hosts.tmp" "${root_mountpoint}/etc/hosts"
cp agent/dist/agent "${root_mountpoint}/usr/local/bin/agent"
chmod 700 "${root_mountpoint}/usr/local/bin/agent"
cp appmgr/target/armv7-unknown-linux-musleabihf/release/appmgr "${root_mountpoint}/usr/local/bin/appmgr"
chmod 700 "${root_mountpoint}/usr/local/bin/appmgr"
cp lifeline/target/armv7-unknown-linux-musleabihf/release/lifeline "${root_mountpoint}/usr/local/bin/lifeline"
chmod 700 "${root_mountpoint}/usr/local/bin/lifeline"
cp docker-daemon.json "${root_mountpoint}/etc/docker/daemon.json"
cp setup.sh "${root_mountpoint}/root/setup.sh"
chmod 700 "${root_mountpoint}/root/setup.sh"
cp setup.service /etc/systemd/system/setup.service
cp lifeline/lifeline.service /etc/systemd/system/lifeline.service
cp agent/config/agent.service /etc/systemd/system/agent.service
cat "${boot_mountpoint}/config.txt" | grep -v "dtoverlay=pwm-2chan" > "${boot_mountpoint}/config.txt.tmp"
echo "dtoverlay=pwm-2chan" >> "${boot_mountpoint}/config.txt.tmp"
umount "${root_mountpoint}"
rm -r "${root_mountpoint}"
umount "${boot_mountpoint}"
rm -r "${boot_mountpoint}"
losetup -d ${loopdev}