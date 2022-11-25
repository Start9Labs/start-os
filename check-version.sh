#!/bin/bash

FE_VERSION="$(cat frontend/package.json | grep -Po '"version":[ \t\n]*"\K[^"]*')"

# TODO: Validate other version sources - backend/Cargo.toml, backend/src/version/mod.rs

VERSION=$FE_VERSION

if ! [ -f ./VERSION.txt ] || [ "$(cat ./VERSION.txt)" != "$VERSION" ]; then
    echo -n "$VERSION" > ./VERSION.txt
fi

echo -n ./VERSION.txt