#!/bin/bash

set -e

rootfs=$1
if [ -z "$rootfs" ]; then
    >&2 echo "usage: $0 <container rootfs path>"
    exit 1
fi

umount --recursive $rootfs
rm -rf $rootfs/..