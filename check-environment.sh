#!/bin/bash

if ! [ -f ./ENVIRONMENT.txt ] || [ "$(cat ./ENVIRONMENT.txt)" != "$ENVIRONMENT" ]; then
    echo -n "$ENVIRONMENT" > ./ENVIRONMENT.txt
fi

echo -n ./ENVIRONMENT.txt