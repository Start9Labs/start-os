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

if ! which pv > /dev/null; then
	>&2 echo 'This script would like to use `pv` to show a progress indicator, but it is not installed.'
	if which apt-get > /dev/null; then
		read -p "Install? [y/N]" -n 1 -r
		echo
		if [[ "$REPLY" =~ ^[Yy]$ ]]; then
			sudo apt-get install pv
		fi
	elif which pacman > /dev/null; then
		read -p "Install? [y/N]" -n 1 -r
		echo
		if [[ "$REPLY" =~ ^[Yy]$ ]]; then
			sudo pacman -S pv
		fi
	else
		>&2 echo 'This script does not recognize what package manager you have available on your system.'
		>&2 echo 'Please go install the utility manually if you want progress reporting.'
	fi
fi

if ! test -e /dev/disk/by-label/green; then
	>&2 echo '`green` partition not found'
	exit 1
fi
export TARGET_PARTITION=$(readlink -f /dev/disk/by-label/green)

if [[ "$TARGET_PARTITION" =~ ^/dev/loop ]]; then
	>&2 echo 'You are currently flashing onto a loop device.'
	>&2 echo 'This is probably a mistake, and usually means you failed to detach a .img file.'
	read -p "Continue anyway? [y/N]" -n 1 -r
	if ! [[ "$REPLY" =~ ^[Yy]$ ]]; then
        exit 1
	else
		echo
	fi
fi

export SOURCE_DEVICE=$(sudo losetup --show -fP eos.img)
sudo e2fsck -f ${SOURCE_DEVICE}p3
sudo resize2fs -M ${SOURCE_DEVICE}p3
export BLOCK_INFO=$(sudo dumpe2fs ${SOURCE_DEVICE}p3)
export BLOCK_COUNT=$(echo "$BLOCK_INFO" | grep "Block count:" | sed 's/Block count:\s\+//g')
export BLOCK_SIZE=$(echo "$BLOCK_INFO" | grep "Block size:" | sed 's/Block size:\s\+//g')
export FS_SIZE=$[$BLOCK_COUNT*$BLOCK_SIZE]
echo "Flashing $FS_SIZE bytes to $TARGET_PARTITION"
if which pv > /dev/null; then
	sudo cat ${SOURCE_DEVICE}p3 | head -c $FS_SIZE | pv -s $FS_SIZE | sudo dd of=${TARGET_PARTITION} bs=1M iflag=fullblock oflag=direct 2>/dev/null
else
	sudo cat ${SOURCE_DEVICE}p3 | head -c $FS_SIZE | sudo dd of=${TARGET_PARTITION} bs=1M iflag=fullblock oflag=direct
fi
echo Verifying...
export INPUT_HASH=$(sudo cat ${SOURCE_DEVICE}p3 | head -c $FS_SIZE | sha256sum)
export OUTPUT_HASH=$(sudo cat ${TARGET_PARTITION} | head -c $FS_SIZE | sha256sum)
sudo losetup -d ${SOURCE_DEVICE}
if ! [[ "$INPUT_HASH" == "$OUTPUT_HASH" ]]; then
	>&2 echo Verification Failed
	exit 1
fi
echo "Verification Succeeded"
