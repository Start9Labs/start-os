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

if [[ "${ENVIRONMENT}" =~ (^|-)unstable($|-) ]]; then
	RUSTFLAGS="--cfg tokio_unstable"
fi

alias 'rust-musl-builder'='docker run $USE_TTY --rm -e "RUSTFLAGS=$RUSTFLAGS" -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$HOME/.cargo/git":/root/.cargo/git -v "$(pwd)":/home/rust/src -w /home/rust/src -P messense/rust-musl-cross:$ARCH-musl'

set +e
fail=
echo "FEATURES=\"$FEATURES\""
echo "RUSTFLAGS=\"$RUSTFLAGS\""
if ! rust-musl-builder sh -c "(cd core && cargo build --release $(if [ -n "$FEATURES" ]; then echo "--features $FEATURES"; fi) --locked --bin startbox --target=$ARCH-unknown-linux-musl)"; then 
	fail=true
fi
if ! rust-musl-builder sh -c "(cd core && cargo build --release --no-default-features --features container-runtime,$FEATURES --locked --bin containerbox --target=$ARCH-unknown-linux-musl)"; then 
	fail=true
fi
set -e
cd core

sudo chown -R $USER target
sudo chown -R $USER ~/.cargo

if [ -n "$fail" ]; then
	exit 1
fi
