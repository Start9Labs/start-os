#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e
shopt -s expand_aliases

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

cd ..
FEATURES="$(echo $ENVIRONMENT | sed 's/-/,/g')"
RUSTFLAGS=""

alias 'rust-gnu-builder'='docker run $USE_TTY --rm -e "RUSTFLAGS=$RUSTFLAGS" -v "$HOME/.cargo/registry":/usr/local/cargo/registry -v "$(pwd)":/home/rust/src -w /home/rust/src -P start9/rust-arm-cross:aarch64'
alias 'rust-musl-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src -P messense/rust-musl-cross:$ARCH-musl'

set +e
fail=
echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
if [ -n "$FEATURES" ]; then
	FEATURES="--features=$FEATURES"
fi
if ! rust-gnu-builder sh -c "(cd core && cargo build --release $FEATURES --locked --bin startbox --target=$ARCH-unknown-linux-gnu)"; then 
	fail=true
fi
for ARCH in x86_64 aarch64
do
	if ! rust-musl-builder sh -c "(cd core && cargo build --release --locked --bin container-init)"; then 
		fail=true
	fi
done
set -e
cd core

sudo chown -R $USER target
sudo chown -R $USER ~/.cargo

if [ -n "$fail" ]; then
	exit 1
fi
