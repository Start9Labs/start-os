#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./install-sdk.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

web="../web/dist/static"
[ -d "$web" ] || mkdir -p "$web"

if [ -z "$PLATFORM" ]; then
  export PLATFORM=$(uname -m)
fi

cargo install --path=. --no-default-features --features=js_engine,sdk,cli --locked
startbox_loc=$(which startbox)
ln -sf $startbox_loc $(dirname $startbox_loc)/start-cli
ln -sf $startbox_loc $(dirname $startbox_loc)/start-sdk
