#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./install-sdk.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

cargo install --bin=embassy-sdk --bin=embassy-cli --path=. --no-default-features --features=js_engine --locked
