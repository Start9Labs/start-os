#!/bin/bash

set -e
shopt -s expand_aliases

if [ "$0" != "./build-sdk.sh" ]; then
	>&2 echo "Must be run from backend directory"
	exit 1
fi

cargo install --bin=embassy-sdk --path=. --no-default-features --verbose