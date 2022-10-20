#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build.sh" ]; then
	>&2 echo "Must be run from compat directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

alias 'rust-musl-builder'='docker run $USE_TTY --rm -v "$HOME"/.cargo/registry:/root/.cargo/registry -v "$(pwd)":/home/rust/src start9/rust-musl-cross:aarch64-musl'

cd ../..
rust-musl-builder sh -c "(git config --global --add safe.directory '*'; cd system-images/compat && cargo +beta build --release --target=aarch64-unknown-linux-musl --no-default-features)"
cd system-images/compat

sudo chown -R $USER target
sudo chown -R $USER ~/.cargo