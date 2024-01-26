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
sudo cp -r dist tmp/combined/usr/lib/startos/init
sudo cp containerRuntime.rc tmp/combined/etc/init.d/containerRuntime
sudo chown -R 0:0 tmp/combined
sudo chmod +x tmp/combined/etc/init.d/containerRuntime
sudo chroot tmp/combined rc-update add containerRuntime default
sudo truncate -s 0 tmp/combined/etc/resolv.conf
rm -f ../build/lib/container-runtime/rootfs.squashfs
mkdir -p ../build/lib/container-runtime
mksquashfs tmp/combined ../build/lib/container-runtime/rootfs.squashfs
sudo umount tmp/combined
sudo umount tmp/lower
sudo rm -rf tmp