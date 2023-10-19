#!/bin/bash

if ! [ -f ./PLATFORM.txt ] || [ "$(cat ./PLATFORM.txt)" != "$PLATFORM" ] && [ -n "$PLATFORM" ]; then
    >&2 echo "Updating PLATFORM.txt to \"$PLATFORM\""
    echo -n "$PLATFORM" > ./PLATFORM.txt
fi

echo -n ./PLATFORM.txt
