#!/bin/bash

set -e

(
    while true; do
        beep -r 2 -l 80 -d 20
        sleep 60
    done
) &

if grep 'cb15ae4d-03' /boot/cmdline.txt; then
    echo Transfer files across
    mkdir -p /media/origin
    mkdir -p /media/dest
    mount -r /dev/mmcblk0p3 /media/origin
    mount -w /dev/mmcblk0p4 /media/dest
    rm -rf /media/dest/*
    rsync -acvAXUH --info=progress2 --delete --force /media/origin/ /media/dest/
    umount /media/origin
    umount /media/dest
    rm -rf /media/origin
    rm -rf /media/dest

    echo Setting up boot to use other partition
    sed -i 's/PARTUUID=cb15ae4d-03/PARTUUID=cb15ae4d-04/g' /boot/cmdline.txt
    sync
    reboot
fi

mkdir -p /media/root-rw
mkfs.ext4 /dev/mmcblk0p3
mount /dev/mmcblk0p3 /media/root-rw

mkdir -p /embassy-os
mount /dev/mmcblk0p2 /embassy-os

mkdir -p /media/root-rw/config
mkdir -p /media/root-rw/current
mkdir -p /media/root-rw/next
rsync -acvAXUH --info=progress2 /embassy-os/ /media/root-rw/config/
rsync -acvAXUH --info=progress2 /update/ /media/root-rw/current/
rsync -acvAXUH --info=progress2 /media/root-rw/current/boot/ /boot/
cp /etc/machine-id /media/root-rw/current/etc/machine-id
cp /etc/ssh/ssh_host_rsa_key /media/root-rw/current/etc/ssh/ssh_host_rsa_key
cp /etc/ssh/ssh_host_rsa_key.pub /media/root-rw/current/etc/ssh/ssh_host_rsa_key.pub
cp /etc/ssh/ssh_host_ecdsa_key /media/root-rw/current/etc/ssh/ssh_host_ecdsa_key
cp /etc/ssh/ssh_host_ecdsa_key.pub /media/root-rw/current/etc/ssh/ssh_host_ecdsa_key.pub
cp /etc/ssh/ssh_host_ed25519_key /media/root-rw/current/etc/ssh/ssh_host_ed25519_key
cp /etc/ssh/ssh_host_ed25519_key.pub /media/root-rw/current/etc/ssh/ssh_host_ed25519_key.pub

sync

umount /embassy-os
umount /media/root-rw

fatlabel /dev/mmcblk0p1 boot
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