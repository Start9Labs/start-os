#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")"

if ! [ -f ./ENVIRONMENT.txt ] || [ "$(cat ./ENVIRONMENT.txt)" != "$ENVIRONMENT" ]; then
    >&2 echo "Updating ENVIRONMENT.txt to \"$ENVIRONMENT\""
    echo -n "$ENVIRONMENT" > ./ENVIRONMENT.txt
fi

echo -n ./build/env/ENVIRONMENT.txt
