#!/bin/bash

set -e

function mktmpfifo () {
	TMP_PATH=$(mktemp)
	rm $TMP_PATH
	mkfifo $TMP_PATH
	echo $TMP_PATH
}

echo 'This script will only work on a card that has previously had a full image written to it.'
echo 'It will *only* flash the ext4 portion (`green` partition) of the img file onto the card.'
echo 'The product key, disk guid, and kernel data will *not* be affected.'
read -p "Continue? [y/N]" -n 1 -r
echo
if ! [[ "$REPLY" =~ ^[Yy]$ ]]; then
        exit 1
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
	elif which brew > /dev/null; then
		read -p "Install? [y/N]" -n 1 -r
		echo
		if [[ "$REPLY" =~ ^[Yy]$ ]]; then
			brew install pv
		fi
	else
		>&2 echo 'This script does not recognize what package manager you have available on your system.'
		>&2 echo 'Please go install the utility manually if you want progress reporting.'
	fi
fi

if [[ "$(uname)" == "Darwin" ]]; then
	export TARGET_PARTITION="disk$(diskutil list | grep EMBASSY | head -1 | rev | cut -b 3)s3"
	if ! test -e $TARGET_PARTITION; then
		>&2 echo '`green` partition not found'
                exit 1
	fi
	export SOURCE_DEVICE="$(hdiutil attach -nomount eos.img | head -n1 | sed -E 's/([^ ]+).*$/\1/g')"
	export SOURCE_PARTITION="${SOURCE_DEVICE}s3"
	function detach () {
		hdiutil detach $SOURCE_DEVICE
	}
else
	if ! test -e /dev/disk/by-label/green; then
		>&2 echo '`green` partition not found'
		exit 1
	fi
	export TARGET_PARTITION=$(readlink -f /dev/disk/by-label/green)
	export SOURCE_DEVICE="$(sudo losetup --show -fP eos.img)"
	export SOURCE_PARTITION="${SOURCE_DEVICE}p3"
	function detach () {
		sudo losetup -d ${SOURCE_DEVICE}
	}
fi

if [[ "$TARGET_PARTITION" =~ ^/dev/loop ]]; then
	>&2 echo 'You are currently flashing onto a loop device.'
	>&2 echo 'This is probably a mistake, and usually means you failed to detach a .img file.'
	read -p "Continue anyway? [y/N]" -n 1 -r
	echo
	if ! [[ "$REPLY" =~ ^[Yy]$ ]]; then
        	exit 1
	fi
fi

if [[ "$(uname)" == "Darwin" ]]; then
	if test -z "$FS_SIZE"; then
		read -p "Enter FS Size (shown during make of eos.img)" -r
		export FS_SIZE=$REPLY
	fi
else
	sudo e2fsck -f ${SOURCE_PARTITION}
	sudo resize2fs -M ${SOURCE_PARTITION}
	export BLOCK_INFO=$(sudo dumpe2fs ${SOURCE_PARTITION})
	export BLOCK_COUNT=$(echo "$BLOCK_INFO" | grep "Block count:" | sed 's/Block count:\s\+//g')
	export BLOCK_SIZE=$(echo "$BLOCK_INFO" | grep "Block size:" | sed 's/Block size:\s\+//g')
	export FS_SIZE=$[$BLOCK_COUNT*$BLOCK_SIZE]
fi
echo "Flashing $FS_SIZE bytes to $TARGET_PARTITION"
if [[ "$(uname)" == "Darwin" ]]; then
	if which pv > /dev/null; then
		sudo cat ${SOURCE_PARTITION} | head -c $FS_SIZE | pv -s $FS_SIZE | sudo dd of=${TARGET_PARTITION} bs=1m 2>/dev/null
	else
		sudo cat ${SOURCE_PARTITION} | head -c $FS_SIZE | sudo dd of=${TARGET_PARTITION} bs=1M iflag=fullblock oflag=direct
	fi
else
	if which pv > /dev/null; then
		sudo cat ${SOURCE_PARTITION} | head -c $FS_SIZE | pv -s $FS_SIZE | sudo dd of=${TARGET_PARTITION} bs=1M iflag=fullblock oflag=direct 2>/dev/null
	else
		sudo cat ${SOURCE_PARTITION} | head -c $FS_SIZE | sudo dd of=${TARGET_PARTITION} bs=1M iflag=fullblock oflag=direct
	fi
fi
echo Verifying...
export INPUT_HASH=$(mktemp)
export OUTPUT_HASH=$(mktemp)
if which pv > /dev/null; then
	export PV_IN=$(mktmpfifo)
fi
sudo cat ${SOURCE_PARTITION} | head -c $FS_SIZE | tee -a $PV_IN | sha256sum > $INPUT_HASH &
export INPUT_CHILD=$!
sudo cat ${TARGET_PARTITION} | head -c $FS_SIZE | tee -a $PV_IN | sha256sum > $OUTPUT_HASH &
export OUTPUT_CHILD=$!
if which pv > /dev/null; then
	pv -s $[$FS_SIZE*2] < $PV_IN > /dev/null &
fi
wait $INPUT_CHILD $OUTPUT_CHILD
if which pv > /dev/null; then
	rm $PV_IN
fi
detach
if ! [[ "$(cat $INPUT_HASH)" == "$(cat $OUTPUT_HASH)" ]]; then
	rm $INPUT_HASH $OUTPUT_HASH
	>&2 echo Verification Failed
	exit 1
fi
rm $INPUT_HASH $OUTPUT_HASH
echo "Verification Succeeded"
