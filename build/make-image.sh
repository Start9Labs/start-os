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
export OUTPUT_DEVICE=$(sudo losetup --show -fP eos.img)
export LOOPDEV=$(sudo losetup --show -fP ubuntu.img)
./build/partitioning.sh
./build/write-image.sh
sudo losetup -d $OUTPUT_DEVICE