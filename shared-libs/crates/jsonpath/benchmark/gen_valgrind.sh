#!/usr/bin/env bash

set -e

cargo clean &&
  cargo build --release && \
  valgrind --tool=massif ./target/release/jsonpath_lib_benches
#  valgrind \
#    --tool=callgrind \
#    --dump-instr=yes \
#    --collect-jumps=yes \
#    --simulate-cache=yes ./target/release/jsonpath_lib_benches

# --simulate-cache=yes $1 -- $2