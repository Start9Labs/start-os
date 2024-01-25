#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

if mountpoint tmp/combined; then sudo umount tmp/combined; fi
if mountpoint tmp/lower; then sudo umount tmp/lower; fi
mkdir -p tmp/lower tmp/upper tmp/work tmp/combined
sudo mount alpine.squashfs tmp/lower
sudo mount -t overlay -olowerdir=tmp/lower,upperdir=tmp/upper,workdir=tmp/work overlay tmp/combined
echo "nameserver 8.8.8.8" | sudo tee tmp/combined/etc/resolv.conf # TODO - delegate to host resolver?
sudo chroot tmp/combined apk add nodejs
sudo mkdir -p tmp/combined/usr/lib/startos/
# cp -r dist tmp/combined/usr/lib/startos/init
# TODO: add init system file
sudo truncate -s 0 tmp/combined/etc/resolv.conf
rm -f ../build/lib/container-runtime/lxc/rootfs.squashfs
mksquashfs tmp/combined ../build/lib/container-runtime/lxc/rootfs.squashfs
sudo umount tmp/combined
sudo umount tmp/lower
sudo rm -rf tmp