#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

rm -rf tmp
mkdir -p tmp/lower tmp/upper tmp/work tmp/combined
sudo mount alpine.squashfs tmp/lower
sudo mount -t overlay -olowerdir=tmp/lower,upperdir=tmp/upper,workdir=tmp/work overlay tmp/combined
echo "nameserver 8.8.8.8" > tmp/combined/etc/resolv.conf # TODO - delegate to host resolver?
sudo chroot . apk add nodejs
mkdir -p tmp/combined/usr/lib/startos/
cp -r dist tmp/combined/usr/lib/startos/init
# TODO: add init system file
truncate tmp/combined/etc/resolv.conf
rm ../build/lib/container-runtime/lxc/rootfs.squashfs
mksquashfs tmp/combined ../build/lib/container-runtime/lxc/rootfs.squashfs