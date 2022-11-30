#!/bin/bash

set -e

function partition_for () {
    if [[ "$1" =~ [0-9]+$ ]]; then
        echo "$1p$2"
    else
        echo "$1$2"
    fi
}

VERSION=$(cat VERSION.txt)
ENVIRONMENT=$(cat ENVIRONMENT.txt)
GIT_HASH=$(cat GIT_HASH.txt | head -c 7)
DATE=$(date +%Y%m%d)

VERSION_FULL="$VERSION-$GIT_HASH"

if [ -n "$ENVIRONMENT" ]; then
  VERSION_FULL="$VERSION_FULL~$ENVIRONMENT"
fi

TARGET_NAME=eos-${VERSION_FULL}-${DATE}_raspberrypi.img
TARGET_SIZE=$[(31116287+1)*512]

rm -f $TARGET_NAME
truncate -s $TARGET_SIZE $TARGET_NAME
(
    echo o
    echo x
    echo i
    echo "0xcb15ae4d"
    echo r
    echo n
    echo p
    echo 1
    echo 2048
    echo 526335
    echo 1050623
    echo n
    echo p
    echo 2
    echo 1050624
    echo 31116287
    echo a
    echo 1
    echo w
) | fdisk $TARGET_NAME
OUTPUT_DEVICE=$(sudo losetup --show -fP $TARGET_NAME)
sudo mkfs.ext4 `partition_for ${OUTPUT_DEVICE} 2`
sudo mkfs.vfat `partition_for ${OUTPUT_DEVICE} 1`

TMPDIR=$(mktemp -d)

sudo mount `partition_for ${OUTPUT_DEVICE} 2` $TMPDIR
sudo mkdir -p $TMPDIR/config
sudo mkdir -p $TMPDIR/next
sudo mkdir -p $TMPDIR/current/boot
sudo mount `partition_for ${OUTPUT_DEVICE} 1` $TMPDIR/current/boot
sudo unsquashfs -f -d $TMPDIR/current eos.raspberrypi.squashfs
sudo umount $TMPDIR/current/boot
sudo umount $TMPDIR
sudo losetup -d $OUTPUT_DEVICE