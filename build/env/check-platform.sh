#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

if ! [ -f ./PLATFORM.txt ] || [ "$(cat ./PLATFORM.txt)" != "$PLATFORM" ] && [ -n "$PLATFORM" ]; then
    >&2 echo "Updating PLATFORM.txt to \"$PLATFORM\""
    echo -n "$PLATFORM" > ./PLATFORM.txt
fi

echo -n ./build/env/PLATFORM.txt
