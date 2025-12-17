#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-cargo-dep.sh" ]; then
	>&2 echo "Must be run from start-os directory"
	exit 1
fi

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

RUST_ARCH="$ARCH"
if [ "$ARCH" = "riscv64" ]; then
  RUST_ARCH="riscv64gc"
fi

mkdir -p cargo-deps

source core/builder-alias.sh

RUSTFLAGS="-C target-feature=+crt-static"

rust-zig-builder cargo-zigbuild install $* --target-dir /workdir/cargo-deps/ --target=$RUST_ARCH-unknown-linux-musl
if [ "$(ls -nd "cargo-deps/$RUST_ARCH-unknown-linux-musl/release/${!#}" | awk '{ print $3 }')" != "$UID" ]; then
  rust-zig-builder sh -c "chown -R $UID:$UID cargo-deps && chown -R $UID:$UID  /usr/local/cargo"
fi
