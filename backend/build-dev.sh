#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-dev.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

USE_TTY=
if tty -s; then
	USE_TTY="-it"
fi

alias 'rust-arm64-builder'='docker run $USE_TTY --rm -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src start9/rust-arm-cross:aarch64'

cd ..
rust-arm64-builder sh -c "(cd backend && cargo build)"
cd backend
#rust-arm64-builder aarch64-linux-gnu-strip target/aarch64-unknown-linux-gnu/release/embassyd
