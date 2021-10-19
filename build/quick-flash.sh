#!/bin/bash

set -e

echo 'This script will only work on a card that has previously had a full image written to it.'
echo 'It will *only* flash the ext4 portion (`green` partition) of the img file onto the card.'
echo 'The product key, disk guid, and kernel data will *not* be affected.'
read -p "Continue? [y/N]" -n 1 -r
if ! [[ "$REPLY" =~ ^[Yy]$ ]]; then
        exit 1
else
	echo
fi

if ! test -e /dev/disk/by-label/green; then
	>&2 echo '`green` partition not found'
	exit 1
fi

export SOURCE_DEVICE=$(sudo losetup --show -fP eos.img)
sudo e2fsck -f ${SOURCE_DEVICE}p3
sudo resize2fs -M ${SOURCE_DEVICE}p3
export BLOCK_INFO=$(sudo dumpe2fs ${SOURCE_DEVICE}p3)
export BLOCK_COUNT=$(echo "$BLOCK_INFO" | grep "Block count:" | sed 's/Block count:\s\+//g')
export BLOCK_SIZE=$(echo "$BLOCK_INFO" | grep "Block size:" | sed 's/Block size:\s\+//g')
sudo dd if=${SOURCE_DEVICE}p3 of=/dev/disk/by-label/green count=$BLOCK_COUNT bs=$BLOCK_SIZE status=progress
echo Verifying...
export INPUT_HASH=$(sudo dd if=${SOURCE_DEVICE}p3 count=$BLOCK_COUNT bs=$BLOCK_SIZE | sha256sum)
export OUTPUT_HASH=$(sudo dd if=/dev/disk/by-label/green count=$BLOCK_COUNT bs=$BLOCK_SIZE | sha256sum)
sudo losetup -d ${SOURCE_DEVICE}
if ! [[ "$INPUT_HASH" == "$OUTPUT_HASH" ]]; then
	>&2 echo Verification Failed
	exit 1
fi
echo Quick Flash Successfull
