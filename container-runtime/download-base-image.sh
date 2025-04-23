#!/bin/bash
cd "$(dirname "${BASH_SOURCE[0]}")"
set -e

DISTRO=debian
VERSION=bookworm
ARCH=${ARCH:-$(uname -m)}
FLAVOR=default

_ARCH=$([[ "$ARCH" = "x86_64" ]] && echo "amd64" || [[ "$ARCH" = "aarch64" ]] && echo "arm64" || echo "$ARCH")

BASE_URL="https://images.linuxcontainers.org$(curl -fsSL https://images.linuxcontainers.org/meta/1.0/index-system | grep "^$DISTRO;$VERSION;$_ARCH;$FLAVOR;" | head -n1 | sed 's/^.*;//g')"
OUTPUT_FILE="debian.${ARCH}.squashfs"

echo "Downloading ${BASE_URL}/rootfs.squashfs to $OUTPUT_FILE"
curl -fsSL "${BASE_URL}/rootfs.squashfs" > "$OUTPUT_FILE"
curl -fsSL "$BASE_URL/SHA256SUMS" | grep 'rootfs.squashfs' | awk '{print $1"  '"$OUTPUT_FILE"'"}' | shasum -a 256 -c