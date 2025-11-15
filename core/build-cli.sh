#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

source ./builder-alias.sh

set -ea
shopt -s expand_aliases

PROFILE=${PROFILE:-release}
if [ "${PROFILE}" = "release" ]; then
	BUILD_FLAGS="--release"
fi

if [ -z "${ARCH:-}" ]; then
  ARCH=$(uname -m)
fi

if [ "$ARCH" = "arm64" ]; then
  ARCH="aarch64"
fi

RUST_ARCH="$ARCH"
if [ "$ARCH" = "riscv64" ]; then
  RUST_ARCH="riscv64gc"
fi

if [ -z "${KERNEL_NAME:-}" ]; then
  KERNEL_NAME=$(uname -s)
fi

if [ -z "${TARGET:-}" ]; then
  if [ "$KERNEL_NAME" = "Linux" ]; then
    TARGET="$RUST_ARCH-unknown-linux-musl"
  elif [ "$KERNEL_NAME" = "Darwin" ]; then
    TARGET="$RUST_ARCH-apple-darwin"
  else
    >&2 echo "unknown kernel $KERNEL_NAME"
    exit 1
  fi
fi

cd ..

# Ensure GIT_HASH.txt exists if not created by higher-level build steps
if [ ! -f GIT_HASH.txt ] && command -v git >/dev/null 2>&1; then
  git rev-parse HEAD > GIT_HASH.txt || true
fi

FEATURES="$(echo "${ENVIRONMENT:-}" | sed 's/-/,/g')"
FEATURE_ARGS="cli"
if [ -n "$FEATURES" ]; then
  FEATURE_ARGS="$FEATURE_ARGS,$FEATURES"
fi

RUSTFLAGS=""
if [[ "${ENVIRONMENT:-}" =~ (^|-)console($|-) ]]; then
  RUSTFLAGS="--cfg tokio_unstable"
fi

echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
rust-zig-builder cargo zigbuild --manifest-path=./core/Cargo.toml $BUILD_FLAGS --no-default-features --features $FEATURE_ARGS --locked --bin start-cli --target=$TARGET
if [ "$(ls -nd "core/target/$TARGET/release/start-cli" | awk '{ print $3 }')" != "$UID" ]; then
  rust-zig-builder sh -c "cd core && chown -R $UID:$UID target && chown -R $UID:$UID /root/.cargo"
fi