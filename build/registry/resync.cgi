#!/bin/bash

declare -A params
while IFS='=' read -r -d '&' key value && [[ -n "$key" ]]; do
    params["$key"]=$value
done <<<"${QUERY_STRING}&"

index_key="${params['key']}"
if [ -z "$index_key" ] || [ "$index_key" != "$(cat /var/www/index_key.txt)" ]; then
    echo "HTTP/1.1 401 UNAUTHORIZED"
    echo "Content-Type: text/html"
    echo
    echo "UNAUTHORIZED"
    exit
fi

touch /tmp/resync

echo "HTTP/1.1 200 OK"
echo "Content-Type: text/html"
echo
echo "OK: Upload successful"