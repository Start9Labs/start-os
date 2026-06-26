#!/bin/bash

# cd lua/bench_lua_vs_rust && ./run.sh

set -e

# http://luajit.org/index.html

# cargo clean && \
cargo build --release

export JSONPATH_LIB_PATH="${PWD}/../target/release/deps"
export LUA_PATH="${PWD}/../?.lua;"

echo
time cargo run --release --bin bench -- 1000
echo
time luajit example.lua 1000
echo
time cargo run --release --bin bench -- 5000
echo
time luajit example.lua 5000
echo
time cargo run --release --bin bench -- 10000
echo
time luajit example.lua 10000

