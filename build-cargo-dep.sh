#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-cargo-dep.sh" ]; then
	>&2 echo "Must be run from embassy-os directory"
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
alias 'rust-arm64-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/usr/local/cargo/registry -v "$(pwd)"/cargo-deps:/home/rust/src -P start9/rust-arm-cross:aarch64'

rust-arm64-builder cargo install "$1" --target-dir /home/rust/src --target=$ARCH-unknown-linux-gnu
sudo chown -R $USER cargo-deps
sudo chown -R $USER ~/.cargo