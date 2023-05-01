#!/bin/bash

set -e
shopt -s expand_aliases

if [ -z "$OS_ARCH" ]; then
	>&2 echo '$OS_ARCH is required'
	exit 1
fi

if [ "$0" != "./build.sh" ]; then
	>&2 echo "Must be run from compat directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

alias 'rust-arm64-musl-builder'='docker run $USE_TTY --rm -e "OS_ARCH=$OS_ARCH" -v "$HOME"/.cargo/registry:/root/.cargo/registry -v "$(pwd)":/home/rust/src messense/rust-musl-cross:aarch64-musl'
alias 'rust-x86_64-musl-builder'='docker run $USE_TTY --rm -e "OS_ARCH=$OS_ARCH" -v "$HOME"/.cargo/registry:/root/.cargo/registry -v "$(pwd)":/home/rust/src messense/rust-musl-cross:x86_64-musl'

cd ../..
rust-arm64-musl-builder sh -c "(git config --global --add safe.directory '*'; cd system-images/compat && cargo build --release --target=aarch64-unknown-linux-musl --no-default-features)"
rust-x86_64-musl-builder sh -c "(git config --global --add safe.directory '*'; cd system-images/compat && cargo build --release --target=x86_64-unknown-linux-musl --no-default-features)"
cd system-images/compat

sudo chown -R $USER target
sudo chown -R $USER ~/.cargo