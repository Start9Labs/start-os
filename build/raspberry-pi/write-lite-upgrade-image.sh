#!/bin/bash

set -e

function partition_for () {
    if [[ "$1" =~ [0-9]+$ ]]; then
        echo "$1p$2"
    else
        echo "$1$2"
    fi
}

# Mount the boot partition and config
mkdir -p /tmp/eos-mnt

sudo mount `partition_for ${OUTPUT_DEVICE} 2` /tmp/eos-mnt

sudo cp ./build/raspberry-pi/033-upgrade.sh /tmp/eos-mnt/usr/local/bin/033-upgrade.sh
sudo cp ./build/raspberry-pi/033-upgrade.service /tmp/eos-mnt/etc/systemd/system/033-upgrade.service
sudo ln -s /etc/systemd/system/033-upgrade.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/033-upgrade.service
sudo cp ./build/raspberry-pi/nc-broadcast.service /tmp/eos-mnt/etc/systemd/system/nc-broadcast.service
sudo ln -s /etc/systemd/system/nc-broadcast.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/nc-broadcast.service

sudo umount /tmp/eos-mnt
