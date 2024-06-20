#!/bin/bash

set -e

IMAGE=$1

if [ -z "$IMAGE" ]; then
    >&2 echo "usage: $0 <image id>"
    exit 1
fi

if ! [ -d "/media/images/$IMAGE" ]; then
    >&2 echo "image does not exist"
    exit 1
fi

container=$(mktemp -d)
mkdir -p $container/rootfs $container/upper $container/work
mount -t overlay -olowerdir=/media/images/$IMAGE,upperdir=$container/upper,workdir=$container/work overlay $container/rootfs

rootfs=$container/rootfs

for special in dev sys proc run; do
    mkdir -p $rootfs/$special
    mount --bind /$special $rootfs/$special
done

echo $rootfs