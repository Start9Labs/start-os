#!/bin/bash

# Enter the backend directory, copy over the built EmbassyOS binaries and systemd services, edit the nginx config, then create the .ssh directory

cp target/aarch64-unknown-linux-gnu/release/embassy-init /mnt/usr/local/bin
cp target/aarch64-unknown-linux-gnu/release/embassyd /mnt/usr/local/bin
cp target/aarch64-unknown-linux-gnu/release/embassy-cli /mnt/usr/local/bin
cp *.service /mnt/etc/systemd/system/

echo "application/wasm		wasm;" | sudo tee -a "/mnt/etc/nginx/mime.types"

mkdir -p /mnt/root/.ssh
