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
TARGET_SIZE=$[(6817791+1)*512]

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
    echo t
    echo c
    echo n
    echo p
    echo 2
    echo 526336
    echo 6817791
    echo a
    echo 1
    echo w
) | fdisk $TARGET_NAME
OUTPUT_DEVICE=$(sudo losetup --show -fP $TARGET_NAME)
sudo mkfs.ext4 `partition_for ${OUTPUT_DEVICE} 2`
sudo mkfs.vfat `partition_for ${OUTPUT_DEVICE} 1`

TMPDIR=$(mktemp -d)

sudo mount `partition_for ${OUTPUT_DEVICE} 2` $TMPDIR
sudo mkdir $TMPDIR/boot
sudo mount `partition_for ${OUTPUT_DEVICE} 1` $TMPDIR/boot
sudo unsquashfs -f -d $TMPDIR eos.raspberrypi.squashfs
REAL_GIT_HASH=$(cat $TMPDIR/usr/lib/embassy/GIT_HASH.txt)
REAL_VERSION=$(cat $TMPDIR/usr/lib/embassy/VERSION.txt)
REAL_ENVIRONMENT=$(cat $TMPDIR/usr/lib/embassy/ENVIRONMENT.txt)
sudo cp ./build/raspberrypi/cmdline.txt $TMPDIR/boot/
sudo cp ./build/raspberrypi/config.txt $TMPDIR/boot/
sudo cp ./build/raspberrypi/fstab $TMPDIR/etc/
sudo mkdir -p $TMPDIR/etc/embassy
sudo cp ./build/raspberrypi/config.yaml $TMPDIR/etc/embassy
sudo cp ./build/raspberrypi/init_resize.sh $TMPDIR//usr/lib/embassy/scripts/init_resize.sh
sudo cp ./cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep $TMPDIR/usr/local/bin/beep
sudo umount $TMPDIR/boot
sudo umount $TMPDIR
sudo losetup -d $OUTPUT_DEVICE

if [ "$ALLOW_VERSION_MISMATCH" != 1 ]; then
    if [ "$(cat GIT_HASH.txt)" != "$REAL_GIT_HASH" ]; then
        >&2 echo "eos.raspberrypi.squashfs GIT_HASH.txt mismatch"
        exit 1
    fi
    if [ "$(cat VERSION.txt)" != "$REAL_VERSION" ]; then
        >&2 echo "eos.raspberrypi.squashfs VERSION.txt mismatch"
        exit 1
    fi
    if [ "$(cat ENVIRONMENT.txt)" != "$REAL_ENVIRONMENT" ]; then
        >&2 echo "eos.raspberrypi.squashfs ENVIRONMENT.txt mismatch"
        exit 1
    fi
fi