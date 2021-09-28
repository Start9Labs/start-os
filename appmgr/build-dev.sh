#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-dev.sh" ]; then
	>&2 echo "Must be run from appmgr directory"
	exit 1
fi

alias 'rust-arm64-builder'='docker run --rm -it -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src start9/rust-arm-cross:aarch64'

cd ../..
rust-arm64-builder sh -c "(cd embassy-os/appmgr && cargo build)"
cd embassy-os/appmgr
#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
