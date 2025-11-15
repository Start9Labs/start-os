#!/bin/bash

USE_TTY=
if tty -s; then
  USE_TTY="-it"
fi

alias 'rust-zig-builder'='docker run '"$USE_TTY"' --rm -e "RUSTFLAGS=$RUSTFLAGS" -e "CFLAGS=-D_FORTIFY_SOURCE=2" -e "CXXFLAGS=-D_FORTIFY_SOURCE=2" -e SCCACHE_GHA_ENABLED -e SCCACHE_GHA_VERSION -e ACTIONS_RESULTS_URL -e ACTIONS_RUNTIME_TOKEN -v "$HOME/.cargo/registry":/usr/local/cargo/registry -v "$HOME/.cargo/git":/root/.cargo/git -v "$HOME/.cache/sccache":/root/.cache/sccache -v "$(pwd)":/workdir -w /workdir -P start9/cargo-zigbuild'
