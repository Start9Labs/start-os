#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-cargo-dep.sh" ]; then
	>&2 echo "Must be run from start-os directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

if [ -z "$ARCH" ]; then
	ARCH=$(uname -m)
fi

mkdir -p cargo-deps
alias 'rust-musl-builder'='docker run $USE_TTY --rm -e "RUSTFLAGS=$RUSTFLAGS" -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)"/cargo-deps:/home/rust/src -w /home/rust/src -P messense/rust-musl-cross:$ARCH-musl'

rust-musl-builder cargo install "$1" --target-dir /home/rust/src --target=$ARCH-unknown-linux-musl
sudo chown -R $USER cargo-deps
sudo chown -R $USER ~/.cargo