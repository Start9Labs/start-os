#!/bin/bash
set -e

echo "Building frontend projects..."
# make clean && make frontend

# ssh start9@10.0.100.196 "mkdir -p /home/start9/temp_web"

echo "Syncing UI to temp directory on remote Embassy...."
rsync -rzP --delete frontend/dist/ui/* start9@10.0.100.196:/home/start9/temp_web

echo "Copying files to distribution directroy..."
ssh start9@10.0.100.196 << EOF
    sudo -i;
    rsync -rzP --delete /home/start9/temp_web/ /var/www/html/main/
EOF