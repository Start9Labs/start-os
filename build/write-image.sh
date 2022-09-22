#!/bin/bash

set -e

function partition_for () {
        if [[ "$1" =~ [0-9]+$ ]]; then
                echo "$1p$2"
        else
                echo "$1$2"
        fi
}

# Write contents of LOOPDEV (Ubuntu image) to sd card and make filesystems, then detach the loop device
echo USING $LOOPDEV TO IMAGE $OUTPUT_DEVICE WITH ENVIRONMENT $ENVIRONMENT
sudo dd if=${LOOPDEV}p1 of=`partition_for ${OUTPUT_DEVICE} 1` bs=1M iflag=fullblock oflag=direct conv=fsync status=progress
sudo mkfs.vfat -F 32 `partition_for ${OUTPUT_DEVICE} 2`
sudo dd if=${LOOPDEV}p2 of=`partition_for ${OUTPUT_DEVICE} 3` bs=1M iflag=fullblock oflag=direct conv=fsync status=progress
sudo mkfs.ext4 `partition_for ${OUTPUT_DEVICE} 4`

sudo losetup -d $LOOPDEV

# Label the filesystems
sudo fatlabel `partition_for ${OUTPUT_DEVICE} 1` system-boot
sudo fatlabel `partition_for ${OUTPUT_DEVICE} 2` EMBASSY
sudo e2label `partition_for ${OUTPUT_DEVICE} 3` green
sudo e2label `partition_for ${OUTPUT_DEVICE} 4` blue

# Mount the boot partition and config
mkdir -p /tmp/eos-mnt
sudo mount `partition_for ${OUTPUT_DEVICE} 1` /tmp/eos-mnt

sudo sed -i 's/PARTUUID=cb15ae4d-02/PARTUUID=cb15ae4d-03/g' /tmp/eos-mnt/cmdline.txt
sudo sed -i 's/ init=\/usr\/lib\/raspi-config\/init_resize.sh//g' /tmp/eos-mnt/cmdline.txt

cat /tmp/eos-mnt/config.txt | grep -v "dtoverlay=" | sudo tee /tmp/eos-mnt/config.txt.tmp > /dev/null
echo "dtoverlay=pwm-2chan,disable-bt" | sudo tee -a /tmp/eos-mnt/config.txt.tmp > /dev/null
echo "gpu_mem=16" | sudo tee -a /tmp/eos-mnt/config.txt.tmp > /dev/null
sudo mv /tmp/eos-mnt/config.txt.tmp /tmp/eos-mnt/config.txt
sudo touch /tmp/eos-mnt/ssh

sudo umount /tmp/eos-mnt

sudo mount `partition_for ${OUTPUT_DEVICE} 3` /tmp/eos-mnt

sudo mkdir /tmp/eos-mnt/media/boot-rw
sudo mkdir /tmp/eos-mnt/embassy-os
sudo mkdir /tmp/eos-mnt/etc/embassy
sudo cp ENVIRONMENT.txt /tmp/eos-mnt/etc/embassy
sudo cp GIT_HASH.txt /tmp/eos-mnt/etc/embassy
sudo cp build/fstab /tmp/eos-mnt/etc/fstab
sudo cp build/journald.conf /tmp/eos-mnt/etc/systemd/journald.conf
sudo sed -i 's/raspberrypi/embassy/g' /tmp/eos-mnt/etc/hostname
sudo sed -i 's/raspberrypi/embassy/g' /tmp/eos-mnt/etc/hosts

# copy over cargo dependencies
sudo cp cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast /tmp/eos-mnt/usr/local/bin

# Enter the backend directory, copy over the built EmbassyOS binaries and systemd services, edit the nginx config, then create the .ssh directory
cd backend/

sudo cp target/aarch64-unknown-linux-gnu/release/embassy-init /tmp/eos-mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassyd /tmp/eos-mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassy-cli /tmp/eos-mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/avahi-alias /tmp/eos-mnt/usr/local/bin
sudo cp *.service /tmp/eos-mnt/etc/systemd/system/

cd ..

# Copy system images
sudo mkdir -p /tmp/eos-mnt/var/lib/embassy/system-images
sudo cp system-images/**/*.tar /tmp/eos-mnt/var/lib/embassy/system-images

# after performing npm run build
sudo mkdir -p /tmp/eos-mnt/var/www/html
sudo cp -R frontend/dist/diagnostic-ui /tmp/eos-mnt/var/www/html/diagnostic
sudo cp -R frontend/dist/setup-wizard /tmp/eos-mnt/var/www/html/setup
sudo cp -R frontend/dist/ui /tmp/eos-mnt/var/www/html/main
sudo cp index.html /tmp/eos-mnt/var/www/html/index.html

# Make the .ssh directory for UID 1000 user
sudo mkdir -p /tmp/eos-mnt/home/$(awk -v val=1000 -F ":" '$3==val{print $1}' /tmp/eos-mnt/etc/passwd)/.ssh
sudo mv /tmp/eos-mnt/etc/sudoers.d/010_pi-nopasswd /tmp/eos-mnt/etc/sudoers.d/010_start9-nopasswd
sudo sed -i 's/pi/start9/g' /tmp/eos-mnt/etc/sudoers.d/010_start9-nopasswd
sudo sed -i 's/ pi / start9 /g' /tmp/eos-mnt/etc/systemd/system/autologin@.service

# Custom MOTD
sudo rm /tmp/eos-mnt/etc/motd
sudo cp ./build/00-embassy /tmp/eos-mnt/etc/update-motd.d
sudo chmod -x /tmp/eos-mnt/etc/update-motd.d/*
sudo chmod +x /tmp/eos-mnt/etc/update-motd.d/00-embassy

if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	cat ./build/initialization.sh | grep -v "passwd -l start9" | sudo tee /tmp/eos-mnt/usr/local/bin/initialization.sh > /dev/null
	sudo chmod +x /tmp/eos-mnt/usr/local/bin/initialization.sh
else
	sudo cp ./build/initialization.sh /tmp/eos-mnt/usr/local/bin
fi
sudo cp ./build/init-with-sound.sh /tmp/eos-mnt/usr/local/bin

sudo cp ./build/initialization.service /tmp/eos-mnt/etc/systemd/system/initialization.service
sudo ln -s /etc/systemd/system/initialization.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/initialization.service
sudo cp ./build/nc-broadcast.service /tmp/eos-mnt/etc/systemd/system/nc-broadcast.service
sudo ln -s /etc/systemd/system/nc-broadcast.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/nc-broadcast.service

sudo umount /tmp/eos-mnt
