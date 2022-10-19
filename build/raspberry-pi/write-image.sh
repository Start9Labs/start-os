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
sudo mount `partition_for ${OUTPUT_DEVICE} 1` /tmp/eos-mnt

cat /tmp/eos-mnt/config.txt | grep -v "dtoverlay=" | sudo tee /tmp/eos-mnt/config.txt.tmp > /dev/null
echo "dtoverlay=pwm-2chan,disable-bt" | sudo tee -a /tmp/eos-mnt/config.txt.tmp > /dev/null
echo "gpu_mem=16" | sudo tee -a /tmp/eos-mnt/config.txt.tmp > /dev/null
sudo mv /tmp/eos-mnt/config.txt.tmp /tmp/eos-mnt/config.txt
sudo touch /tmp/eos-mnt/ssh

sudo umount /tmp/eos-mnt

sudo mount `partition_for ${OUTPUT_DEVICE} 2` /tmp/eos-mnt

sudo mkdir /tmp/eos-mnt/media/embassy/
sudo make install ARCH=aarch64 DESTDIR=/tmp/eos-mnt
sudo sed -i 's/raspberrypi/embassy/g' /tmp/eos-mnt/etc/hostname
sudo sed -i 's/raspberrypi/embassy/g' /tmp/eos-mnt/etc/hosts
sudo cp cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast /tmp/eos-mnt/usr/local/bin
sudo cp backend/*.service /tmp/eos-mnt/etc/systemd/system/
sudo mkdir -p /tmp/eos-mnt/etc/embassy
sudo cp build/raspberry-pi/config.yaml /tmp/eos-mnt/etc/embassy/config.yaml

# Make the .ssh directory for UID 1000 user
sudo mkdir -p /tmp/eos-mnt/home/$(awk -v val=1000 -F ":" '$3==val{print $1}' /tmp/eos-mnt/etc/passwd)/.ssh
sudo mv /tmp/eos-mnt/etc/sudoers.d/010_pi-nopasswd /tmp/eos-mnt/etc/sudoers.d/010_start9-nopasswd
sudo sed -i 's/pi/start9/g' /tmp/eos-mnt/etc/sudoers.d/010_start9-nopasswd
sudo sed -i 's/ pi / start9 /g' /tmp/eos-mnt/etc/systemd/system/autologin@.service

if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	cat ./build/raspberry-pi/initialization.sh | grep -v "passwd -l start9" | sudo tee /tmp/eos-mnt/usr/local/bin/initialization.sh > /dev/null
	sudo chmod +x /tmp/eos-mnt/usr/local/bin/initialization.sh
else
	sudo cp ./build/raspberry-pi/initialization.sh /tmp/eos-mnt/usr/local/bin
fi
sudo cp ./build/raspberry-pi/init-with-sound.sh /tmp/eos-mnt/usr/local/bin

sudo cp ./build/raspberry-pi/initialization.service /tmp/eos-mnt/etc/systemd/system/initialization.service
sudo ln -s /etc/systemd/system/initialization.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/initialization.service
sudo cp ./build/raspberry-pi/nc-broadcast.service /tmp/eos-mnt/etc/systemd/system/nc-broadcast.service
sudo ln -s /etc/systemd/system/nc-broadcast.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/nc-broadcast.service

sudo umount /tmp/eos-mnt
