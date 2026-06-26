#!/bin/bash

set -e
DIR="$(pwd)"

cd "${DIR}"/bench_bin && cargo build --release

ITER=100000

printf "\n\n$..book[?(@.price<30 && @.category=="fiction")] (loop ${ITER})"
printf "\n\n"

__default () {
    echo "Rust - select: " && time ./bench.sh select ${ITER}
    printf "\n"
    sleep 1
    cd "${DIR}"/javascript && echo "NodeJs - jsonpath - query: " && time ./bench.sh jsonpath ${ITER}
    printf "\n"
    sleep 1
    cd "${DIR}"/javascript && echo "NodeJs - jsonpath-wasm - select:" && time ./bench.sh wasmSelect ${ITER}
}

__extra () {
    echo "Rust - selector: " && time ./bench.sh selector ${ITER}
    printf "\n"
    sleep 1
    echo "Rust - compile: " && time ./bench.sh compile ${ITER}
    printf "\n"
    sleep 1
    cd "${DIR}"/javascript && echo "NodeJs - jsonpath - query: " && time ./bench.sh jsonpath ${ITER}
    printf "\n"
    sleep 1
    cd "${DIR}"/javascript && echo "NodeJs - jsonpath-wasm - selector: " && time ./bench.sh wasmSelector ${ITER}
    printf "\n"
    sleep 1
    cd "${DIR}"/javascript && echo "NodeJs - jsonpath-wasm - compile: " && time ./bench.sh wasmCompile ${ITER}
    printf "\n"
    sleep 1
    cd "${DIR}"/javascript && echo "NodeJs - jsonpath-wasm - Selector: " && time ./bench.sh wasmSelectorClass ${ITER}
    printf "\n"
}

if [ "$1" = "extra" ]; then
    __extra
else
    __default
fi