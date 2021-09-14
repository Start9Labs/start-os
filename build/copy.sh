#!/bin/bash

# Enter the appmgr directory, copy over the built EmbassyOS binaries and systemd services, edit the nginx config, then create the .ssh directory
cd appmgr/

sudo cp target/aarch64-unknown-linux-gnu/release/embassy-init /mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassyd /mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassy-cli /mnt/usr/local/bin
sudo cp *.service /mnt/etc/systemd/system/

echo "application/wasm		wasm;" | sudo tee -a "/mnt/etc/nginx/mime.types"

sudo mkdir -p /mnt/root/.ssh
