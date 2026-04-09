#!/bin/bash
set -ea
shopt -s expand_aliases

cd "$(git rev-parse --show-toplevel)"

source build/builder-alias.sh

ARCH=${ARCH:-riscv64}
RUST_ARCH=${RUST_ARCH:-riscv64gc}
PROFILE=${PROFILE:-release}
if [ -z "$STARTWRT_GIT_HASH" ]; then
    STARTWRT_GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
    git diff-index --quiet HEAD -- 2>/dev/null || STARTWRT_GIT_HASH="${STARTWRT_GIT_HASH}-dirty"
fi
export STARTWRT_GIT_HASH

if [ "${PROFILE}" = "release" ]; then
	BUILD_FLAGS="--release"
fi

echo "==> Building Rust binaries (arch=$RUST_ARCH, profile=$PROFILE)..."
rust-zig-builder cargo zigbuild \
    --manifest-path=./backend/ctrl/Cargo.toml \
    $BUILD_FLAGS \
    --locked \
    --target=${RUST_ARCH}-unknown-linux-musl

# Fix ownership if built in Docker as root
if [ "$(ls -nd "backend/target/$RUST_ARCH-unknown-linux-musl/$PROFILE/startwrt" | awk '{ print $3 }')" != "$UID" ]; then
  rust-zig-builder sh -c "chown -R $UID:$UID backend/target && chown -R $UID:$UID /usr/local/cargo"
fi

echo "==> Rust build complete."
