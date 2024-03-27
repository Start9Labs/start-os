#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

DISTRO=alpine
VERSION=3.19
ARCH=${ARCH:-$(uname -m)}
FLAVOR=default

if [ "$ARCH" = "x86_64" ]; then
    ARCH=amd64
elif [ "$ARCH" = "aarch64" ]; then
    ARCH=arm64
fi

curl https://images.linuxcontainers.org/$(curl --silent https://images.linuxcontainers.org/meta/1.0/index-system | grep "^$DISTRO;$VERSION;$ARCH;$FLAVOR;" | head -n1 | sed 's/^.*;//g')/rootfs.squashfs --output alpine.squashfs