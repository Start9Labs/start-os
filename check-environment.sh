#!/bin/bash

if ! [ -f ./ENVIRONMENT.txt ] || [ "$(cat ./ENVIRONMENT.txt)" != "$ENVIRONMENT" ]; then
    >&2 echo "Updating ENVIRONMENT.txt to $ENVIRONMENT"
    echo -n "$ENVIRONMENT" > ./ENVIRONMENT.txt
fi

echo -n ./ENVIRONMENT.txt
