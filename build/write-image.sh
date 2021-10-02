#!/bin/bash

set -e

# Write contents of LOOPDEV (Ubuntu image) to sd card and make filesystems, then detach the loop device
echo USING $LOOPDEV TO IMAGE $OUTPUT_DEVICE
sudo dd if=${LOOPDEV}p1 of=${OUTPUT_DEVICE}p1 bs=4096 conv=fsync status=progress
sudo mkfs.vfat -F 32 ${OUTPUT_DEVICE}p2
sudo dd if=${LOOPDEV}p2 of=${OUTPUT_DEVICE}p3 bs=4096 conv=fsync status=progress
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

sudo sed -i 's/LABEL=writable/LABEL=green/g' /tmp/eos-mnt/cmdline.txt
cat /tmp/eos-mnt/config.txt | grep -v "dtoverlay=" | sudo tee /tmp/eos-mnt/config.txt.tmp
echo "dtoverlay=pwm-2chan" | sudo tee -a /tmp/eos-mnt/config.txt.tmp
sudo mv /tmp/eos-mnt/config.txt.tmp /tmp/eos-mnt/config.txt

# Unmount the boot partition and mount embassy partition
sudo umount /tmp/eos-mnt
sudo mount ${OUTPUT_DEVICE}p2 /tmp/eos-mnt
sudo cp product_key.txt /tmp/eos-mnt
sudo umount /tmp/eos-mnt

sudo mount ${OUTPUT_DEVICE}p3 /tmp/eos-mnt
# Enter the appmgr directory, copy over the built EmbassyOS binaries and systemd services, edit the nginx config, then create the .ssh directory
cd appmgr/

sudo cp target/aarch64-unknown-linux-gnu/release/embassy-init /tmp/eos-mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassyd /tmp/eos-mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassy-cli /tmp/eos-mnt/usr/local/bin
sudo cp *.service /tmp/eos-mnt/etc/systemd/system/

cd ..

# after performing npm run build
sudo mkdir -p /tmp/eos-mnt/var/www/html
sudo cp -R ui/www /tmp/eos-mnt/var/www/html/main
sudo cp -R setup-wizard/www /tmp/eos-mnt/var/www/html/setup
sudo cp -R diagnostic-ui/www /tmp/eos-mnt/var/www/html/diagnostic

# Make the .ssh directory
sudo mkdir -p /tmp/eos-mnt/root/.ssh

sudo cp ./build/initialization.sh /tmp/eos-mnt/usr/local/bin
sudo cp ./build/initialization.service /tmp/eos-mnt/etc/systemd/system/initialization.service
sudo ln -s  /etc/systemd/system/initialization.service /tmp/eos-mnt/etc/systemd/system/multi-user.target.wants/initialization.service

sudo umount /tmp/eos-mnt
