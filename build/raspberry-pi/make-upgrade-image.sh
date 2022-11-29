#!/bin/bash

set -e

function partition_for () {
    if [[ "$1" =~ [0-9]+$ ]]; then
        echo "$1p$2"
    else
        echo "$1$2"
    fi
}

TARGET_NAME=lite-upgrade.img
TARGET_SIZE=7000000000

LOOPDEV=$(sudo losetup --show -fP raspios.img)
sudo cat `partition_for ${LOOPDEV} 2` > $TARGET_NAME
sudo losetup -d $LOOPDEV
truncate -s $TARGET_SIZE $TARGET_NAME
sudo e2fsck -f -y $TARGET_NAME
sudo resize2fs $TARGET_NAME

TMPDIR=$(mktemp -d)

sudo mount $TARGET_NAME $TMPDIR/

sudo mkdir -p $TMPDIR/update
sudo unsquashfs -f -d $TMPDIR/update eos.raspberrypi.squashfs
sudo cp ./build/raspberry-pi/033-upgrade.sh $TMPDIR/usr/local/bin/033-upgrade.sh
sudo cp ./build/raspberry-pi/033-upgrade.service $TMPDIR/etc/systemd/system/033-upgrade.service
sudo ln -s /etc/systemd/system/033-upgrade.service $TMPDIR/etc/systemd/system/multi-user.target.wants/033-upgrade.service
sudo cp ./build/raspberry-pi/nc-broadcast.service $TMPDIR/etc/systemd/system/nc-broadcast.service
sudo ln -s /etc/systemd/system/nc-broadcast.service $TMPDIR/etc/systemd/system/multi-user.target.wants/nc-broadcast.service

sudo umount $TMPDIR/

sudo e2fsck -f -y $TARGET_NAME
sudo resize2fs -M $TARGET_NAME
BLOCK_INFO=$(sudo dumpe2fs $TARGET_NAME)
BLOCK_COUNT=$(echo "$BLOCK_INFO" | grep "Block count:" | sed 's/Block count:\s\+//g')
BLOCK_SIZE=$(echo "$BLOCK_INFO" | grep "Block size:" | sed 's/Block size:\s\+//g')
FS_SIZE=$[$BLOCK_COUNT*$BLOCK_SIZE]
truncate -s $FS_SIZE $TARGET_NAME