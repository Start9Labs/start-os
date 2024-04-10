#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e



if mountpoint tmp/combined; then sudo umount -R tmp/combined; fi
if mountpoint tmp/lower; then sudo umount tmp/lower; fi
mkdir -p tmp/lower tmp/upper tmp/work tmp/combined
sudo mount debian.${ARCH}.squashfs tmp/lower
sudo mount -t overlay -olowerdir=tmp/lower,upperdir=tmp/upper,workdir=tmp/work overlay tmp/combined
sudo mount --bind /dev tmp/combined/dev

QEMU=
if [ "$ARCH" != "$(uname -m)" ]; then
    QEMU=/usr/bin/qemu-${ARCH}-static
    sudo cp $(which qemu-$ARCH-static) tmp/combined${QEMU}
fi

sudo mkdir -p tmp/combined/run/systemd/resolve
echo "nameserver 8.8.8.8" | sudo tee tmp/combined/run/systemd/resolve/stub-resolv.conf
sudo chroot tmp/combined $QEMU /usr/bin/apt update
sudo chroot tmp/combined $QEMU /usr/bin/apt install -y nodejs rsync
sudo mkdir -p tmp/combined/usr/lib/startos/
sudo rsync -a --copy-unsafe-links dist/ tmp/combined/usr/lib/startos/init/
sudo cp container-runtime.service tmp/combined/lib/systemd/system/container-runtime.service
sudo chroot tmp/combined $QEMU /usr/bin/systemctl enable container-runtime.service
sudo cp ../core/target/$ARCH-unknown-linux-musl/release/containerbox tmp/combined/usr/bin/start-cli

if [ -n "$QEMU" ]; then
    sudo rm tmp/combined${QEMU}
fi

sudo truncate -s 0 tmp/combined/etc/resolv.conf
sudo chown -R 0:0 tmp/combined
rm -f rootfs.${ARCH}.squashfs
mkdir -p ../build/lib/container-runtime
sudo mksquashfs tmp/combined rootfs.${ARCH}.squashfs
sudo umount tmp/combined/dev
sudo umount tmp/combined
sudo umount tmp/lower
sudo rm -rf tmp