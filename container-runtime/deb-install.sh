#!/bin/bash

set -e

mkdir -p /run/systemd/resolve
echo "nameserver 8.8.8.8" > /run/systemd/resolve/stub-resolv.conf

apt-get update
apt-get install -y curl rsync

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
source ~/.bashrc
nvm install 20

systemctl enable container-runtime.service

rm -rf /run/systemd