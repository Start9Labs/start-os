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

mkdir -p $TMPDIR/source
mkdir -p $TMPDIR/target

truncate -s 5000000000 update.img
sudo mount update.img $TMPDIR/target/

sudo mount $ROOT_PARTITION $TMPDIR/source/
sudo mount $BOOT_PARTITION $TMPDIR/source/current/boot/
sudo rsync -acvAXH --info=progress2 $TMPDIR/source/current/ $TMPDIR/target/

sudo sed -i 's/PARTUUID=[a-f0-9]+/PARTUUID=cb15ae4d/g' $TMPDIR/target/etc/fstab
sudo sed -i 's/PARTUUID=[a-f0-9]+/PARTUUID=cb15ae4d/g' $TMPDIR/target/boot/cmdline.txt

sudo umount $TMPDIR/source/current/boot/
sudo umount $TMPDIR/source/
sudo umount $TMPDIR/target

rm -rf $TMPDIR

echo "Compressing..."
if which pv > /dev/null; then
	cat update.img | pv -s $FS_SIZE | gzip > update.img.gz
else
	cat update.img | gzip > update.img.gz
fi
