#!/bin/bash

FE_VERSION="$(cat frontend/package.json | grep '"version"' | sed 's/[ \t]*"version":[ \t]*"\([^"]*\)",/\1/')"

# TODO: Validate other version sources - backend/Cargo.toml, backend/src/version/mod.rs

VERSION=$FE_VERSION

if ! [ -f ./VERSION.txt ] || [ "$(cat ./VERSION.txt)" != "$VERSION" ]; then
    echo -n "$VERSION" > ./VERSION.txt
fi

echo -n ./VERSION.txt