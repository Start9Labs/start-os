#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-portable-dev.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

alias 'rust-musl-builder'='docker run --rm -v "$HOME"/.cargo/registry:/root/.cargo/registry -v "$(pwd)":/home/rust/src start9/rust-musl-cross:x86_64-musl'

cd ..
rust-musl-builder sh -c "(cd backend && cargo +beta build --target=x86_64-unknown-linux-musl --no-default-features)"
cd backend
