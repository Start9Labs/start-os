#!/bin/bash

set -e

rm -rf frontend/dist/static

find frontend/dist/raw -type f -not -name '*.gz' -and -not -name '*.br' | xargs -n 1 -P 0 gzip -kf
find frontend/dist/raw -type f -not -name '*.gz' -and -not -name '*.br' | xargs -n 1 -P 0 brotli -kf

for file in $(find frontend/dist/raw -type f -not -name '*.gz' -and -not -name '*.br'); do
    raw_size=$(expr $(du  $file | awk '{print $1}') \* 512)
    gz_size=$(expr $(du  $file.gz | awk '{print $1}') \* 512 )
    br_size=$(expr $(du  $file.br | awk '{print $1}') \* 512 )
    if [ $((gz_size * 100 / raw_size)) -gt 70 ]; then
        rm $file.gz
    fi
    if [ $((br_size * 100 / raw_size)) -gt 70 ]; then
        rm $file.br
    fi
done

cp -r frontend/dist/raw frontend/dist/static