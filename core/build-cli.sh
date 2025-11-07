#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

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

if [ -z "${KERNEL_NAME:-}" ]; then
  KERNEL_NAME=$(uname -s)
fi

if [ -z "${TARGET:-}" ]; then
  if [ "$KERNEL_NAME" = "Linux" ]; then
    TARGET="$ARCH-unknown-linux-musl"
  elif [ "$KERNEL_NAME" = "Darwin" ]; then
    TARGET="$ARCH-apple-darwin"
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
cross build --manifest-path=./core/Cargo.toml $BUILD_FLAGS --no-default-features --features $FEATURE_ARGS --locked --bin start-cli --target=$TARGET