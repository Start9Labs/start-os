#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -ea
shopt -s expand_aliases

web="../web/dist/static"
[ -d "$web" ] || mkdir -p "$web"

if [ -z "$PLATFORM" ]; then
  PLATFORM=$(uname -m)
fi

if [ "$PLATFORM" = "arm64" ]; then
  PLATFORM="aarch64"
fi

cargo install --path=./startos --no-default-features --features=cli,docker,registry --bin start-cli --locked
