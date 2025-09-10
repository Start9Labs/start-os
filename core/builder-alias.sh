#!/bin/bash

alias 'rust-musl-builder'='docker run $USE_TTY --rm -e "RUSTFLAGS=$RUSTFLAGS" -e SCCACHE_GHA_ENABLED -e SCCACHE_GHA_VERSION -e ACTIONS_RESULTS_URL -e ACTIONS_RUNTIME_TOKEN -v "$HOME/.cargo/registry":/root/.cargo/registry -v "$HOME/.cargo/git":/root/.cargo/git -v "$HOME/.cache/sccache":/root/.cache/sccache -v "$(pwd)":/home/rust/src -w /home/rust/src -P start9/rust-musl-cross:$ARCH-musl'
