#!/bin/bash

# get size of /var/lib/docker and add space to it
SIZE_DOCKER_LIB=$(du -s /var/lib/docker)
SIZE=$(($SIZE_DOCKER_LIB + 5000000))

# Allocate a file to be used as a block device
truncate -size=$SIZE docker_lib_file

# Create a pool with that file as a storage backend
zpool create docker-pool docker_lib_file

# Create a zfs fs on that pool
zpool create docker-pool/main

# Copy /var/lib/docker onto that fs
zpool add docker_pool/main /var/lib/docker

# Mount /var/lib/docker to that fs
zfs get -H -ovalue mountpoint docker-pool

zfs set mountpoint=/var/lib/docker docker_pool

zfs mount docker_poool/main

# Write docker.json file
echo '{ "storage-driver": "zfs" }' > /etc/docker/daemon.json

# Restart docker
systemctl restart docker

# Load images
docker load --input /root/compat.tar

# persisit load response for temporary debugging
docker images > /root/images.out

# Copy all /var/lib/docker onto actual ext4 fs:
# First, copy to tmp place that is not /var/lib/docker
sudo mkdir /root/tmp_docker
sudo cp -r /var/lib/docker /root/tmp_docker

# Unmount (full pool export) or pool destroy
zpool destroy docker_pool

# Clear old data
rm -rf /var/lib/docker/*

# Move to actual /var/lib/docker 
mv /root/tmp_docker/* /var/lib/docker/*

# Clean up
rm -rf /root/tmp_docker

# Delete file used as block device
rm docker_lib_file

