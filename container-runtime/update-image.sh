#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

if mountpoint tmp/combined; then sudo umount -R tmp/combined; fi
if mountpoint tmp/lower; then sudo umount tmp/lower; fi
sudo rm -rf tmp
mkdir -p tmp/lower tmp/upper tmp/work tmp/combined
if which squashfuse > /dev/null; then
    sudo squashfuse debian.${ARCH}.squashfs tmp/lower
else
    sudo mount debian.${ARCH}.squashfs tmp/lower
fi
sudo mount -t overlay -olowerdir=tmp/lower,upperdir=tmp/upper,workdir=tmp/work overlay tmp/combined

QEMU=
if [ "$ARCH" != "$(uname -m)" ]; then
    QEMU=/usr/bin/qemu-${ARCH}-static
    if ! which qemu-$ARCH-static > /dev/null; then
        >&2 echo qemu-user-static is required for cross-platform builds
        sudo umount tmp/combined
        sudo umount tmp/lower
        sudo rm -rf tmp
        exit 1
    fi
    sudo cp $(which qemu-$ARCH-static) tmp/combined${QEMU}
fi

sudo mkdir -p tmp/combined/usr/lib/startos/
sudo rsync -a --copy-unsafe-links dist/ tmp/combined/usr/lib/startos/init/
sudo chown -R 0:0 tmp/combined/usr/lib/startos/
sudo cp container-runtime.service tmp/combined/lib/systemd/system/container-runtime.service
sudo chown 0:0 tmp/combined/lib/systemd/system/container-runtime.service
sudo cp ../core/target/$ARCH-unknown-linux-musl/release/containerbox tmp/combined/usr/bin/start-cli
sudo chown 0:0 tmp/combined/usr/bin/start-cli
echo container-runtime | sha256sum | head -c 32 | cat - <(echo) | sudo tee tmp/combined/etc/machine-id
cat deb-install.sh | sudo systemd-nspawn --console=pipe -D tmp/combined $QEMU /bin/bash
sudo truncate -s 0 tmp/combined/etc/machine-id

if [ -n "$QEMU" ]; then
    sudo rm tmp/combined${QEMU}
fi

rm -f rootfs.${ARCH}.squashfs
mkdir -p ../build/lib/container-runtime
sudo mksquashfs tmp/combined rootfs.${ARCH}.squashfs
sudo umount tmp/combined
sudo umount tmp/lower
sudo rm -rf tmp