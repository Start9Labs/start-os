#!/bin/bash

# Enter the appmgr directory, copy over the built EmbassyOS binaries and systemd services, edit the nginx config, then create the .ssh directory
cd appmgr/

sudo cp target/aarch64-unknown-linux-gnu/release/embassy-init /mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassyd /mnt/usr/local/bin
sudo cp target/aarch64-unknown-linux-gnu/release/embassy-cli /mnt/usr/local/bin
sudo cp *.service /mnt/etc/systemd/system/

# Make the frontend directories, enter the embassy-os directory, copy over the 3 ui components
mkdir /mnt/var/www
mkdir /mnt/var/www/html
cd ..

sudo cp -R ui/ /mnt/var/www/html/
sudo cp -R setup-wizard/ /mnt/var/www/html/
sudo cp -R diagnostic-ui/ /mnt/var/www/html/

# Make the .ssh directory
sudo mkdir -p /mnt/root/.ssh
