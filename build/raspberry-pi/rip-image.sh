#!/bin/bash

set -e

TMPDIR=$(mktemp -d)

ROOT_PARTITION=$(readlink -f /dev/disk/by-label/rootfs)
BOOT_PARTITION=$(readlink -f /dev/disk/by-label/boot)

if [[ "$ROOT_PARTITION" =~ ^/dev/loop ]] || [[ "$BOOT_PARTITION" =~ ^/dev/loop ]]; then
	>&2 echo 'You are currently ripping from a loop device.'
	>&2 echo 'This is probably a mistake, and usually means you failed to detach a .img file.'
	read -p "Continue anyway? [y/N]" -n 1 -r
	echo
	if ! [[ "$REPLY" =~ ^[Yy]$ ]]; then
        	exit 1
	fi
fi

sudo mount $ROOT_PARTITION $TMPDIR/
sudo mount $BOOT_PARTITION $TMPDIR/current/boot/
sudo sed -i 's/PARTUUID=[a-f0-9]\+/PARTUUID=cb15ae4d/g' $TMPDIR/current/etc/fstab
sudo sed -i 's/PARTUUID=[a-f0-9]\+/PARTUUID=cb15ae4d/g' $TMPDIR/current/boot/cmdline.txt
rm -f eos.raspberrypi.squashfs
sudo mksquashfs $TMPDIR/current/ eos.raspberrypi.squashfs

sudo umount $TMPDIR/current/boot/
sudo umount $TMPDIR/

rm -rf $TMPDIR
