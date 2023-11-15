#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

PLATFORM=$1

if [ -z "$PLATFORM" ]; then
	>&2 echo "usage: $0 <PLATFORM>"
	exit 1
fi

mkdir -p ./firmware/$PLATFORM

for firmware_id in $(jq --raw-output ".[] | select(.platform[] | contains(\"$PLATFORM\")) | .id" ./build/lib/firmware.json); do
	dest=./firmware/$PLATFORM/${firmware_id}.rom.gz
	curl --fail -L -o $dest "$(jq --raw-output ".[] | select(.id == \"${firmware_id}\") | .url" ./build/lib/firmware.json)"
done
