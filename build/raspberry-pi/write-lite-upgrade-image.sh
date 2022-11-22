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
TMPDIR=$(mktemp -d)
mkdir $TMPDIR/target
mkdir $TMPDIR/source

sudo mount update.img $TMPDIR/source

sudo mount `partition_for ${OUTPUT_DEVICE} 2` $TMPDIR/target

sudo mkdir -p $TMPDIR/target/update
sudo rsync -acvAXH $TMPDIR/source/ $TMPDIR/target/update/
sudo cp ./build/raspberry-pi/033-upgrade.sh $TMPDIR/target/usr/local/bin/033-upgrade.sh
sudo cp ./build/raspberry-pi/033-upgrade.service $TMPDIR/target/etc/systemd/system/033-upgrade.service
sudo ln -s /etc/systemd/system/033-upgrade.service $TMPDIR/target/etc/systemd/system/multi-user.target.wants/033-upgrade.service
sudo cp ./build/raspberry-pi/nc-broadcast.service $TMPDIR/target/etc/systemd/system/nc-broadcast.service
sudo ln -s /etc/systemd/system/nc-broadcast.service $TMPDIR/target/etc/systemd/system/multi-user.target.wants/nc-broadcast.service

sudo umount $TMPDIR/target
