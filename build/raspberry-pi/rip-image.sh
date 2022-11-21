#!/bin/bash

set -e

TMPDIR=$(mktemp -d)

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
	>&2 echo 'OSX not supported'
	exit 1
fi

if ! test -e /dev/disk/by-label/rootfs; then
	>&2 echo '`rootfs` partition not found'
	exit 1
fi
ROOT_PARTITION=$(readlink -f /dev/disk/by-label/rootfs)
BOOT_PARTITION=$(readlink -f /dev/disk/by-label/boot)

if [[ "$ROOT_PARTITION" =~ ^/dev/loop ]] || [[ "$BOOT_PARTITION" =~ ^/dev/loop ]]; then
	>&2 echo 'You are currently ripping from a loop device.'
	>&2 echo 'This is probably a mistake, and usually means you failed to detach a .img file.'
	read -p "Continue anyway? [y/N]" -n 1 -r
	echo
	if ! [[ "$REPLY" =~ ^[Yy]$ ]]; then
        	exit 1
	fi
fi

sudo e2fsck -f ${ROOT_PARTITION}
sudo resize2fs -M ${ROOT_PARTITION}
BLOCK_INFO=$(sudo dumpe2fs ${ROOT_PARTITION})
BLOCK_COUNT=$(echo "$BLOCK_INFO" | grep "Block count:" | sed 's/Block count:\s\+//g')
BLOCK_SIZE=$(echo "$BLOCK_INFO" | grep "Block size:" | sed 's/Block size:\s\+//g')
FS_SIZE=$[$BLOCK_COUNT*$BLOCK_SIZE]

echo "Ripping $FS_SIZE bytes from $ROOT_PARTITION"
if which pv > /dev/null; then
	sudo cat ${ROOT_PARTITION} | head -c $FS_SIZE | pv -s $FS_SIZE | sudo dd of=update.img bs=1M iflag=fullblock oflag=direct conv=fsync 2>/dev/null 
else
	sudo cat ${ROOT_PARTITION} | head -c $FS_SIZE | sudo dd of=update.img bs=1M iflag=fullblock oflag=direct conv=fsync
fi
echo Verifying...
INPUT_HASH=$TMPDIR/input.hash
OUTPUT_HASH=$TMPDIR/output.hash
if which pv > /dev/null; then
	PV_IN=$TMPDIR/fifo
	mkfifo $PV_IN
fi
sudo cat ${ROOT_PARTITION} | head -c $FS_SIZE | tee -a $PV_IN | sha256sum > $INPUT_HASH &
INPUT_CHILD=$!
sudo cat update.img | head -c $FS_SIZE | tee -a $PV_IN | sha256sum > $OUTPUT_HASH &
OUTPUT_CHILD=$!
if which pv > /dev/null; then
	pv -s $[$FS_SIZE*2] < $PV_IN > /dev/null &
fi
wait $INPUT_CHILD $OUTPUT_CHILD
if which pv > /dev/null; then
	rm $PV_IN
fi
if ! [[ "$(cat $INPUT_HASH)" == "$(cat $OUTPUT_HASH)" ]]; then
	rm $INPUT_HASH $OUTPUT_HASH
	>&2 echo Verification Failed
	exit 1
fi
rm $INPUT_HASH $OUTPUT_HASH
echo "Verification Succeeded"

mkdir $TMPDIR/rootmnt
mkdir $TMPDIR/bootmnt
sudo mount update.img $TMPDIR/rootmnt
sudo mount $BOOT_PARTITION $TMPDIR/bootmnt
rsync -acvAXH --info=progress2 $TMPDIR/bootmnt/ $TMPDIR/rootmnt/boot/

echo "Compressing..."
if which pv > /dev/null; then
	cat update.img | pv -s $FS_SIZE | gzip > update.img.gz
else
	cat update.img | gzip > update.img.gz
fi
