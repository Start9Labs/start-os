#!/bin/bash

PROFILE=${PROFILE:-release}
if [ "${PROFILE}" = "release" ]; then
	BUILD_FLAGS="--release"
fi

cd "$(dirname "${BASH_SOURCE[0]}")"

set -ea
shopt -s expand_aliases

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

if [ "$ARCH" = "arm64" ]; then
  ARCH="aarch64"
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

cd ..
FEATURES="$(echo $ENVIRONMENT | sed 's/-/,/g')"
RUSTFLAGS=""

if [[ "${ENVIRONMENT}" =~ (^|-)unstable($|-) ]]; then
	RUSTFLAGS="--cfg tokio_unstable"
fi

source ./core/builder-alias.sh

echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
rust-musl-builder sh -c "cd core && cargo build $BUILD_FLAGS --no-default-features --features cli,startd,$FEATURES --locked --bin startbox --target=$ARCH-unknown-linux-musl"
if [ "$(ls -nd core/target/$ARCH-unknown-linux-musl/${PROFILE}/startbox | awk '{ print $3 }')" != "$UID" ]; then
	rust-musl-builder sh -c "cd core && chown -R $UID:$UID target && chown -R $UID:$UID /root/.cargo"
fi