#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

DISTRO=alpine
VERSION=3.19
ARCH=${ARCH:-$(uname -m)}
FLAVOR=default

_ARCH=$ARCH
if [ "$_ARCH" = "x86_64" ]; then
    _ARCH=amd64
elif [ "$_ARCH" = "aarch64" ]; then
    _ARCH=arm64
fi

curl https://images.linuxcontainers.org/$(curl --silent https://images.linuxcontainers.org/meta/1.0/index-system | grep "^$DISTRO;$VERSION;$_ARCH;$FLAVOR;" | head -n1 | sed 's/^.*;//g')/rootfs.squashfs --output alpine.${ARCH}.squashfs