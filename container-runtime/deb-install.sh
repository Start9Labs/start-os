#!/bin/bash

set -e

mkdir -p /run/systemd/resolve
echo "nameserver 8.8.8.8" > /run/systemd/resolve/stub-resolv.conf

apt-get update
apt-get install -y curl rsync qemu-user-static

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
source ~/.bashrc
nvm install 20
ln -s $(which node) /usr/bin/node

sed -i '/\(^\|#\)Storage=/c\Storage=persistent' /etc/systemd/journald.conf
sed -i '/\(^\|#\)Compress=/c\Compress=yes' /etc/systemd/journald.conf
sed -i '/\(^\|#\)SystemMaxUse=/c\SystemMaxUse=1G' /etc/systemd/journald.conf
sed -i '/\(^\|#\)ForwardToSyslog=/c\ForwardToSyslog=no' /etc/systemd/journald.conf

systemctl enable container-runtime.service

rm -rf /run/systemd