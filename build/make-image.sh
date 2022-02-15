#!/bin/bash

set -e

SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
cd $DIR/..

truncate --size=$[(31116287+1)*512] eos.img
if [ -z "$OUTPUT_DEVICE" ]; then
  export OUTPUT_DEVICE=$(sudo losetup --show -fP eos.img)
  export DETACH_OUTPUT_DEVICE=1
else
  export DETACH_OUTPUT_DEVICE=0
  sudo dd if=/dev/zero of=$OUTPUT_DEVICE bs=1M count=1
fi
export LOOPDEV=$(sudo losetup --show -fP raspios.img)
./build/partitioning.sh
./build/write-image.sh
sudo e2fsck -f ${OUTPUT_DEVICE}p3
sudo resize2fs -M ${OUTPUT_DEVICE}p3
BLOCK_INFO=$(sudo dumpe2fs ${OUTPUT_DEVICE}p3)
BLOCK_COUNT=$(echo "$BLOCK_INFO" | grep "Block count:" | sed 's/Block count:\s\+//g')
BLOCK_SIZE=$(echo "$BLOCK_INFO" | grep "Block size:" | sed 's/Block size:\s\+//g')
echo "YOUR GREEN FILESYSTEM is '$[$BLOCK_COUNT*$BLOCK_SIZE]' BYTES"
echo "IF YOU ARE QUICK-FLASHING FROM MAC-OS, NOTE THIS NUMBER FOR LATER"
if [ "$DETACH_OUTPUT_DEVICE" -eq "1" ]; then
  sudo losetup -d $OUTPUT_DEVICE
fi
