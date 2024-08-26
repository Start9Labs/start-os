#!/bin/bash

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

alias 'rust-musl-builder'='docker run $USE_TTY --rm -e "RUSTFLAGS=$RUSTFLAGS" -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$HOME/.cargo/git":/root/.cargo/git -v "$(pwd)":/home/rust/src -w /home/rust/src -P messense/rust-musl-cross:$ARCH-musl'

echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
rust-musl-builder sh -c "apt-get update && apt-get install -y rsync && cd core && cargo test --release --features=test,$FEATURES --workspace --locked --target=$ARCH-unknown-linux-musl -- --skip export_bindings_ && chown \$UID:\$UID target"
if [ "$(stat -c '%u' core/target)" != "$UID" ]; then
	rust-musl-builder sh -c "cd core && chown -R $UID:$UID target && chown -R $UID:$UID /root/.cargo"
fi