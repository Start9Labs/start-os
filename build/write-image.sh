#!/bin/bash

set -e

# Write contents of LOOPDEV (Ubuntu image) to sd card and make filesystems, then detach the loop device
echo USING $LOOPDEV TO IMAGE $OUTPUT_DEVICE
sudo dd if=${LOOPDEV}p1 of=${OUTPUT_DEVICE}p1 bs=1M iflag=fullblock oflag=direct conv=fsync status=progress
sudo mkfs.vfat -F 32 ${OUTPUT_DEVICE}p2
sudo dd if=${LOOPDEV}p2 of=${OUTPUT_DEVICE}p3 bs=1M iflag=fullblock oflag=direct conv=fsync status=progress
sudo mkfs.ext4 ${OUTPUT_DEVICE}p4

sudo losetup -d $LOOPDEV

# Label the filesystems
sudo fatlabel ${OUTPUT_DEVICE}p1 system-boot
sudo fatlabel ${OUTPUT_DEVICE}p2 EMBASSY
sudo e2label ${OUTPUT_DEVICE}p3 green
sudo e2label ${OUTPUT_DEVICE}p4 blue

# Mount the boot partition and config
mkdir -p /tmp/eos-mnt
sudo mount ${OUTPUT_DEVICE}p1 /tmp/eos-mnt

if [[ "$ENVIRONMENT" =~ (^|-)dev($|-) ]]; then
	sudo cp build/user-data-dev /tmp/eos-mnt/user-data
else
	sudo cp build/user-data /tmp/eos-mnt/user-data
fi

sudo sed -i 's/LABEL=writable/LABEL=green/g' /tmp/eos-mnt/cmdline.txt
# create a copy of the cmdline *without* the quirk string, so that it can be easily amended
sudo cp /tmp/eos-mnt/cmdline.txt /tmp/eos-mnt/cmdline.txt.orig
sudo sed -i 's/^/usb-storage.quirks=152d:0562:u /g' /tmp/eos-mnt/cmdline.txt

cat /tmp/eos-mnt/config.txt | grep -v "dtoverlay=" | sudo tee /tmp/eos-mnt/config.txt.tmp
echo "dtoverlay=pwm-2chan,disable-bt" | sudo tee -a /tmp/eos-mnt/config.txt.tmp
sudo mv /tmp/eos-mnt/config.txt.tmp /tmp/eos-mnt/config.txt

# Unmount the boot partition and mount embassy partition
sudo umount /tmp/eos-mnt
sudo mount ${OUTPUT_DEVICE}p2 /tmp/eos-mnt
sudo touch /tmp/eos-mnt/system-rebuild # rebuild docker state on first startup
if [ "$NO_KEY" != "1" ]; then sudo cp product_key.txt /tmp/eos-mnt; else echo "This image is being written with no product key"; fi
sudo umount /tmp/eos-mnt

sudo mount ${OUTPUT_DEVICE}p3 /tmp/eos-mnt

sudo mkdir  /tmp/eos-mnt/media/boot-rw
sudo mkdir  /tmp/eos-mnt/embassy-os
sudo cp build/fstab /tmp/eos-mnt/etc/fstab
# Enter the backend directory, copy over the built EmbassyOS binaries and systemd services, edit the nginx config, then create the .ssh directory
cd backend/

sudo cp target/aarch64-unknown-linux-gnu/release/embassy-init /tmp/eos-mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassyd /tmp/eos-mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassy-cli /tmp/eos-mnt/usr/local/bin
sudo cp *.service /tmp/eos-mnt/etc/systemd/system/

cd ..

# Copy system images
sudo mkdir -p /tmp/eos-mnt/var/lib/embassy/system-images
sudo cp system-images/**/*.tar /tmp/eos-mnt/var/lib/embassy/system-images

# after performing npm run build
sudo mkdir -p /tmp/eos-mnt/var/www/html
sudo cp -R ui/www /tmp/eos-mnt/var/www/html/main
sudo cp -R setup-wizard/www /tmp/eos-mnt/var/www/html/setup
sudo cp -R diagnostic-ui/www /tmp/eos-mnt/var/www/html/diagnostic

# Make the .ssh directory
sudo mkdir -p /tmp/eos-mnt/root/.ssh

sudo cp ./build/initialization.sh /tmp/eos-mnt/usr/local/bin

sudo cp ./build/initialization.service /tmp/eos-mnt/etc/systemd/system/initialization.service
sudo ln -s /etc/systemd/system/initialization.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/initialization.service

sudo umount /tmp/eos-mnt
