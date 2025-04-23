#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

DISTRO=debian
VERSION=bookworm
ARCH=${ARCH:-$(uname -m)}
FLAVOR=default

_ARCH=$ARCH
if [ "$_ARCH" = "x86_64" ]; then
    _ARCH=amd64
elif [ "$_ARCH" = "aarch64" ]; then
    _ARCH=arm64
fi

BASE_URL="https://images.linuxcontainers.org/$(curl -fsSL https://images.linuxcontainers.org/meta/1.0/index-system | grep "^$DISTRO;$VERSION;$_ARCH;$FLAVOR;" | head -n1 | sed 's/^.*;//g')"
URL="$BASE_URL/rootfs.squashfs"

echo "Downloading $URL to debian.${ARCH}.squashfs"

curl -fsSL "$URL" > debian.${ARCH}.squashfs
curl -fsSL "$BASE_URL/SHA256SUMS" | grep ' rootfs\.squashfs$' | sed 's/rootfs\.squashfs/'"debian.${ARCH}.squashfs/" | sha256sum -c