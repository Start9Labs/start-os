#!/bin/bash

set -e
shopt -s expand_aliases

alias 'rust-arm-builder'='docker run --rm -it -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$(pwd)":/home/rust/src start9/rust-arm-cross:latest'

cd ..
rust-arm-builder sh -c "(cd appmgr && cargo build --release --features=production)"
cd appmgr
rust-arm-builder arm-linux-gnueabi-strip target/armv7-unknown-linux-gnueabihf/release/appmgr