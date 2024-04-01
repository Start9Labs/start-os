#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e
shopt -s expand_aliases

web="../web/dist/static"
[ -d "$web" ] || mkdir -p "$web"

if [ -z "$PLATFORM" ]; then
  export PLATFORM=$(uname -m)
fi

cargo install --path=./startos --no-default-features --features=js-engine,sdk,cli --locked
startbox_loc=$(which startbox)
ln -sf $startbox_loc $(dirname $startbox_loc)/start-cli
ln -sf $startbox_loc $(dirname $startbox_loc)/start-sdk
