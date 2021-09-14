#!/bin/bash

# Write contents of LOOPDEV (Ubuntu image) to sd card and make filesystems, then detach the loop device
sudo dd if=${LOOPDEV}p1 of=/dev/$(echo $OUTPUT_DEVICE)p1 status=progress
sudo mkfs.vfat /dev/$(echo $OUTPUT_DEVICE)p2
sudo dd if=${LOOPDEV}p2 of=/dev/$(echo $OUTPUT_DEVICE)p3 status=progress
sudo mkfs.ext4 /dev/$(echo $OUTPUT_DEVICE)p4

sudo losetup -d $LOOPDEV

# Label the filesystems
sudo fatlabel /dev/$(echo $OUTPUT_DEVICE)p1 system-boot
sudo fatlabel /dev/$(echo $OUTPUT_DEVICE)p2 EMBASSY
sudo e2label /dev/$(echo $OUTPUT_DEVICE)p3 writable
sudo e2label /dev/$(echo $OUTPUT_DEVICE)p4 reserved

# Mount the boot partition and config
sudo mount /dev/$(echo $OUTPUT_DEVICE)p1 /mnt

cat "/mnt/config.txt" | grep -v "dtoverlay=" | sudo tee "/mnt/config.txt.tmp"
echo "dtoverlay=pwm-2chan" | sudo tee -a "/mnt/config.txt.tmp"
sudo mv "/mnt/config.txt.tmp" "/mnt/config.txt"

# Unmount the boot partition and mount embassy partition
sudo umount /mnt
sudo mount /dev/$(echo $OUTPUT_DEVICE)p2 /mnt
