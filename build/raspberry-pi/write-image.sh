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
sudo mount `partition_for ${OUTPUT_DEVICE} 1` $TMPDIR

cat $TMPDIR/config.txt | grep -v "dtoverlay=" | sudo tee $TMPDIR/config.txt.tmp > /dev/null
echo "dtoverlay=pwm-2chan,disable-bt" | sudo tee -a $TMPDIR/config.txt.tmp > /dev/null
echo "gpu_mem=16" | sudo tee -a $TMPDIR/config.txt.tmp > /dev/null
sudo mv $TMPDIR/config.txt.tmp $TMPDIR/config.txt
sudo touch $TMPDIR/ssh

sudo umount $TMPDIR

sudo mount `partition_for ${OUTPUT_DEVICE} 2` $TMPDIR

sudo mkdir $TMPDIR/media/embassy/
sudo ENVIRONMENT=$ENVIRONMENT make V=1 install ARCH=aarch64 OS_ARCH=raspberrypi DESTDIR=$TMPDIR --debug
sudo sed -i 's/raspberrypi/embassy/g' $TMPDIR/etc/hostname
sudo sed -i 's/raspberrypi/embassy/g' $TMPDIR/etc/hosts
sudo cp cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast $TMPDIR/usr/local/bin
sudo cp backend/*.service $TMPDIR/etc/systemd/system/
sudo mkdir -p $TMPDIR/etc/embassy
sudo cp build/raspberry-pi/config.yaml $TMPDIR/etc/embassy/config.yaml

# Make the .ssh directory for UID 1000 user
sudo mkdir -p $TMPDIR/home/$(awk -v val=1000 -F ":" '$3==val{print $1}' $TMPDIR/etc/passwd)/.ssh
sudo mv $TMPDIR/etc/sudoers.d/010_pi-nopasswd $TMPDIR/etc/sudoers.d/010_start9-nopasswd
sudo sed -i 's/pi/start9/g' $TMPDIR/etc/sudoers.d/010_start9-nopasswd
sudo sed -i 's/ pi / start9 /g' $TMPDIR/etc/systemd/system/autologin@.service

if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
    cat ./build/raspberry-pi/initialization.sh | grep -v "passwd -l start9" | sudo tee $TMPDIR/usr/local/bin/initialization.sh > /dev/null
    sudo chmod +x $TMPDIR/usr/local/bin/initialization.sh
else
    sudo cp ./build/raspberry-pi/initialization.sh $TMPDIR/usr/local/bin
fi
sudo cp ./build/raspberry-pi/init-with-sound.sh $TMPDIR/usr/local/bin

sudo cp ./build/raspberry-pi/initialization.service $TMPDIR/etc/systemd/system/initialization.service
sudo ln -s /etc/systemd/system/initialization.service $TMPDIR/etc/systemd/system/multi-user.target.wants/initialization.service
sudo cp ./build/raspberry-pi/nc-broadcast.service $TMPDIR/etc/systemd/system/nc-broadcast.service
sudo ln -s /etc/systemd/system/nc-broadcast.service $TMPDIR/etc/systemd/system/multi-user.target.wants/nc-broadcast.service

sudo umount $TMPDIR
