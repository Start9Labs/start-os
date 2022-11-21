#!/bin/bash

set -e

function partition_for () {
    if [[ "$1" =~ [0-9]+$ ]]; then
        echo "$1p$2"
    else
        echo "$1$2"
    fi
}

TARGET_NAME=embassyos-raspi.img
if [ "$LITE_UPGRADE_IMAGE" = "1" ]; then
    TARGET_NAME=lite-upgrade.img
fi

cp raspios.img $TARGET_NAME
truncate -s 3000000000 $TARGET_NAME
(
    echo d
    echo 2
    echo n
    echo p
    echo 2
    echo 532480
    echo
    echo w
) | fdisk $TARGET_NAME
export OUTPUT_DEVICE=$(sudo losetup --show -fP $TARGET_NAME)
sudo e2fsck -f -y `partition_for ${OUTPUT_DEVICE} 2`
sudo resize2fs `partition_for ${OUTPUT_DEVICE} 2`
if [ "$LITE_UPGRADE_IMAGE" = "1" ]; then
    ./build/raspberry-pi/write-lite-upgrade-image.sh
else
    ./build/raspberry-pi/write-image.sh
fi
sudo e2fsck -f -y `partition_for ${OUTPUT_DEVICE} 2`
sudo resize2fs -M `partition_for ${OUTPUT_DEVICE} 2`
sudo losetup -d $OUTPUT_DEVICE
