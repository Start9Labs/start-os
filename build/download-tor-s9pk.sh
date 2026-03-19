#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

ARCH=$1
VERSION="0.4.9.5:0-beta.0"

if [ -z "$ARCH" ]; then
	>&2 echo "usage: $0 <ARCH>"
	exit 1
fi

curl --fail -L -o "./lib/tor_${ARCH}.s9pk" "https://s9pks.nyc3.cdn.digitaloceanspaces.com/tor/${VERSION}/tor_${ARCH}.s9pk"
