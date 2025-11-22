#!/bin/bash

USE_TTY=
if tty -s; then
  USE_TTY="-it"
fi

alias 'rust-zig-builder'='docker run '"$USE_TTY"' --rm -e "RUSTFLAGS=$RUSTFLAGS" -e "AWS_LC_SYS_CMAKE_TOOLCHAIN_FILE_riscv64gc_unknown_linux_musl=/root/cmake-overrides/toolchain-riscv64-musl-clang.cmake" -e SCCACHE_GHA_ENABLED -e SCCACHE_GHA_VERSION -e ACTIONS_RESULTS_URL -e ACTIONS_RUNTIME_TOKEN -v "$HOME/.cargo/registry":/usr/local/cargo/registry -v "$HOME/.cargo/git":/root/.cargo/git -v "$HOME/.cache/sccache":/root/.cache/sccache -v "$(pwd)":/workdir -w /workdir -P start9/cargo-zigbuild'
