#!/bin/bash
set -e

cargo clean && cargo build --release && \
  perf record --call-graph dwarf ./target/release/jsonpath_lib_benches && \
  perf script | stackcollapse-perf.pl | stackcollapse-recursive.pl | rust-unmangle | flamegraph.pl > flame.svg