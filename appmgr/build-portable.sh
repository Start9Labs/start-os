#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-portable.sh" ]; then
	>&2 echo "Must be run from appmgr directory"
	exit 1
fi

alias 'rust-musl-builder'='docker run --rm -it -v "$HOME"/.cargo/registry:/root/.cargo/registry -v "$(pwd)":/home/rust/src messense/rust-musl-cross:x86_64-musl'

cd ..
rust-musl-builder sh -c "(cd appmgr && cargo build --release --target=x86_64-unknown-linux-musl --features=portable,production --no-default-features)"
cd appmgr
