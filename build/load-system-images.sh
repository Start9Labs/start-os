#!/bin/bash

# get size of /var/lib/docker and add space to it
SIZE_DOCKER_LIB=$(du -s /var/lib/docker)
SIZE=$(($SIZE_DOCKER_LIB + 5000000))

# Allocate a file to be used as a block device
sudo truncate -size=$SIZE docker_lib_file

# Create a pool with that file as a storage backend
sudo zpool create docker-pool docker_lib_file

# Create a zfs fs on that pool
sudo zfs create docker-pool/main

# Copy /var/lib/docker onto that fs
sudo cp -r /var/lib/docker /docker-pool/main

# Mount /var/lib/docker to that fs
sudo mount --bind /docker-pool/main/docker /var/lib/docker

# Write docker.json file
sudo echo '{ "storage-driver": "zfs" }' > /etc/docker/daemon.json

# Restart docker
sudo systemctl restart docker

# Load images
sudo docker load --input /root/compat.tar

# persisit load response for temporary debugging
sudo docker images > /root/images.out

# Copy all /var/lib/docker onto actual ext4 fs:
# First, copy to tmp place that is not /var/lib/docker
sudo mkdir /root/tmp_docker
sudo cp -r /var/lib/docker /root/tmp_docker

# Unmount (full pool export) or pool destroy
sudo zpool destroy docker_pool

# Clear old data
sudo rm -rf /var/lib/docker

# Move to actual /var/lib/docker 
sudo mv /root/tmp_docker /var/lib/docker

# Delete file used as block device
sudo rm docker_lib_file

