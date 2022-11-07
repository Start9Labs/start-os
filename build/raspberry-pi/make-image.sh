#!/bin/bash

set -e

function partition_for () {
    if [[ "$1" =~ [0-9]+$ ]]; then
        echo "$1p$2"
    else
        echo "$1$2"
    fi
}

cp raspios.img embassyos-raspi.img
truncate -s 3000000000 embassyos-raspi.img
(
    echo d
    echo 2
    echo n
    echo p
    echo 2
    echo 532480
    echo
    echo w
) | fdisk embassyos-raspi.img
export OUTPUT_DEVICE=$(sudo losetup --show -fP embassyos-raspi.img)
sudo e2fsck -f -y `partition_for ${OUTPUT_DEVICE} 2`
sudo resize2fs `partition_for ${OUTPUT_DEVICE} 2`
./build/raspberry-pi/write-image.sh
sudo e2fsck -f -y `partition_for ${OUTPUT_DEVICE} 2`
sudo resize2fs -M `partition_for ${OUTPUT_DEVICE} 2`
sudo losetup -d $OUTPUT_DEVICE
