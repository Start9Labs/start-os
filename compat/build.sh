#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build.sh" ]; then
	>&2 echo "Must be run from compat directory"
	exit 1
fi

alias 'rust-musl-builder'='docker run --rm -it -v "$HOME"/.cargo/registry:/root/.cargo/registry -v "$(pwd)":/home/rust/src start9/rust-musl-cross:aarch64-musl'

cd ../..
rust-musl-builder sh -c "(cd embassy-os/compat && cargo +beta build --release --target=aarch64-unknown-linux-musl --no-default-features)"
cd embassy-os/compat
