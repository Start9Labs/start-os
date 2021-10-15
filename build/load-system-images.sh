#!/bin/bash

# get size of /var/lib/docker and add space to it
SIZE_DOCKER_LIB=$(du -s /var/lib/docker | cut -d/ -f1)
SIZE_SYS_IMG=$()

# make size in mb - 64M is min needed for zfs
SIZE=$((($SIZE_DOCKER_LIB + 512 / 1024) + 64 + 200))

# Allocate a file to be used as a block device
truncate --size ${SIZE}MB /dev/docker_lib_file

# Create a pool with that file as a storage backend
zpool create docker-pool /dev/docker_lib_file

# Create a zfs fs on that pool
zfs create docker-pool/main

# Copy /var/lib/docker onto that fs
cp -r /var/lib/docker /docker-pool/main

# Mount /var/lib/docker to that fs
mount --bind /docker-pool/main/docker /var/lib/docker

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
cp -r /var/lib/docker /root/tmp_docker

# Unmount
systemctl stop docker
umount /var/lib/docker

# Destroy pool, which also exports
zpool destroy docker-pool

# Clear old data
rm -rf /var/lib/docker

# Move to actual /var/lib/docker 
mv /root/tmp_docker /var/lib/docker

# Delete file used as block device
rm /dev/docker_lib_file

# Start Docker again
echo '{ "storage-driver": "overlay2" }' > /etc/docker/daemon.json
systemctl start docker