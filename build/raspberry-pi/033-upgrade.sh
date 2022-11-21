#!/bin/bash

set -e

if grep 'cb15ae4d-03' /media/boot-rw/cmdline.txt; then
    BLOCK_COUNT=$(tune2fs -l /dev/mmcblk0p3 | grep "^Block count:" | awk '{print $3}')
    BLOCK_SIZE=$(tune2fs -l /dev/mmcblk0p3 | grep "^Block size:" | awk '{print $3}')
    cat /dev/mmcblk0p3 | head -c $[$BLOCK_COUNT * $BLOCK_SIZE] > /dev/mmcblk0p4
    sed -i 's/PARTUUID=cb15ae4d-03/PARTUUID=cb15ae4d-04/g' /media/boot-rw/cmdline.txt
    sync
    reboot
fi

e2fsck -f /dev/mmcblk0p3
resize2fs /dev/mmcblk0p3

umount /embassy-os
mkdir -p /media/root-rw
mount /dev/mmcblk0p3 /media/root-rw

mkdir -p /media/root-rw/config
mkdir -p /media/root-rw/current
mkdir -p /media/root-rw/next
while ! rsync -acvAXUH --delete --force --info=progress2 alpha-registry-x.start9.com::0.3.3/raspberrypi/ /media/root-rw/current/; do
    >&2 echo "Failed to sync rootfs, retrying in 10s"
    sleep 10
done
cp /etc/machine-id /media/root-rw/current/etc/machine-id
cp /etc/ssh/ssh_host_rsa_key /media/root-rw/current/etc/ssh/ssh_host_rsa_key
cp /etc/ssh/ssh_host_rsa_key.pub /media/root-rw/current/etc/ssh/ssh_host_rsa_key.pub
cp /etc/ssh/ssh_host_ecdsa_key /media/root-rw/current/etc/ssh/ssh_host_ecdsa_key
cp /etc/ssh/ssh_host_ecdsa_key.pub /media/root-rw/current/etc/ssh/ssh_host_ecdsa_key.pub
cp /etc/ssh/ssh_host_ed25519_key /media/root-rw/current/etc/ssh/ssh_host_ed25519_key
cp /etc/ssh/ssh_host_ed25519_key.pub /media/root-rw/current/etc/ssh/ssh_host_ed25519_key.pub
rsync -acvAXUH --info=progress2 /media/root-rw/current/boot/ /media/boot-rw/

sync

umount /media/root-rw

e2label /dev/mmcblk0p3 rootfs

(
    echo d
    echo 1
    echo d
    echo 2
    echo n
    echo p
    echo 1
    echo
    echo
    echo d
    echo 3
    echo d
    echo 4
    echo n
    echo p
    echo 2
    echo
    echo
    echo t
    echo 1
    echo c
    echo w
) | fdisk /dev/mmcblk0

reboot