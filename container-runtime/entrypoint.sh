#!/bin/sh

for image in $(ls -d /media/images/*); do
    for special in dev sys proc; do
        mkdir -p $image/$special
        mount --bind /$special $image/$special
    done
done

exec node /usr/local/lib/startInit.js