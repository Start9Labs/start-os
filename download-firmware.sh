#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

set -e

PLATFORM=$1

if [ -z "$PLATFORM" ]; then
	>&2 echo "usage: $0 <PLATFORM>"
	exit 1
fi

rm -rf ./firmware/$PLATFORM
mkdir -p ./firmware/$PLATFORM

cd ./firmware/$PLATFORM

for firmware_id in $(jq --raw-output ".[] | select(.platform[] | contains(\"$PLATFORM\")) | .id" ../../build/lib/firmware.json); do
	curl --fail -L -o ${firmware_id}.rom.gz "$(jq --raw-output ".[] | select(.id == \"${firmware_id}\") | .url" ../../build/lib/firmware.json)"
done
 
sha256sum * > checksums.sha256
