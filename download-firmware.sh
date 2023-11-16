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

mapfile -t firmwares <<< "$(jq -c ".[] | select(.platform[] | contains(\"$PLATFORM\"))" ../../build/lib/firmware.json)"
for firmware in "${firmwares[@]}"; do
	if [ -n "$firmware" ]; then
		id=$(echo "$firmware" | jq --raw-output '.id')
		url=$(echo "$firmware" | jq --raw-output '.url')
		shasum=$(echo "$firmware" | jq --raw-output '.shasum')
		curl --fail -L -o "${id}.rom.gz" "$url"
		echo "$shasum ${id}.rom.gz" | sha256sum -c
	fi
done
