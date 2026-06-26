#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")/.."

set -e
shopt -s expand_aliases

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

RUST_ARCH="$ARCH"
if [ "$ARCH" = "riscv64" ]; then
  RUST_ARCH="riscv64gc"
fi

mkdir -p target

source shared-libs/crates/start-core/build/builder-alias.sh

RUSTFLAGS="-C target-feature=+crt-static"

# pi-beep is vendored in-repo (workspace member); others install from crates.io.
INSTALL_SPEC="$*"
if [ "$1" = "pi-beep" ]; then
  INSTALL_SPEC="--path shared-libs/crates/pi-beep"
fi
rust-zig-builder cargo-zigbuild install $INSTALL_SPEC --target-dir /workdir/target/ --target=$RUST_ARCH-unknown-linux-musl
if [ "$(ls -nd "target/$RUST_ARCH-unknown-linux-musl/release/${!#}" | awk '{ print $3 }')" != "$UID" ]; then
  rust-zig-builder sh -c "chown -R $UID:$UID target && chown -R $UID:$UID  /usr/local/cargo"
fi
