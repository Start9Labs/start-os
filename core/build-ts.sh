#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

source ./builder-alias.sh

set -ea
shopt -s expand_aliases

PROFILE=${PROFILE:-release}
if [ "${PROFILE}" = "release" ]; then
	BUILD_FLAGS="--release"
fi

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

if [ "$ARCH" = "arm64" ]; then
	ARCH="aarch64"
fi

RUST_ARCH="$ARCH"
if [ "$ARCH" = "riscv64" ]; then
	RUST_ARCH="riscv64gc"
fi

cd ..
FEATURES="$(echo $ENVIRONMENT | sed 's/-/,/g')"
RUSTFLAGS=""
if [[ "${ENVIRONMENT}" =~ (^|-)console($|-) ]]; then
	RUSTFLAGS="--cfg tokio_unstable"
fi
echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
rust-zig-builder cargo test --manifest-path=./core/Cargo.toml $BUILD_FLAGS --no-default-features --features test,$FEATURES --locked 'export_bindings_'
if [ "$(ls -nd "core/startos/bindings" | awk '{ print $3 }')" != "$UID" ]; then
  rust-zig-builder sh -c "chown -R $UID:$UID core/target && chown -R $UID:$UID core/startos/bindings && chown -R $UID:$UID /root/.cargo"
fi