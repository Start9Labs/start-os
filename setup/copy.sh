#!/bin/bash

# Copy over the built EmbassyOS binaries and systemd services, then create the .ssh directory
sudo cp target/aarch64-unknown-linux-gnu/release/embassy-init /mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassyd /mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassy-cli /mnt/usr/local/bin
sudo cp *.service /mnt/etc/systemd/system/

sudo mkdir -p /mnt/root/.ssh
