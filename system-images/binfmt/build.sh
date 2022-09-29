#!/bin/bash

set -e

docker run -d -p5000:5000 --rm --name registry registry
docker buildx build --platform linux/amd64 --push -t 172.17.0.1:5000/binfmt:amd64 .
docker buildx build --platform linux/arm64 --push -t 172.17.0.1:5000/binfmt:arm64 .
docker manifest create --amend --insecure 172.17.0.1:5000/binfmt:latest 172.17.0.1:5000/binfmt:amd64 172.17.0.1:5000/binfmt:arm64
docker manifest push --insecure 172.17.0.1:5000/binfmt:latest
docker pull --platform=linux/arm64 --platform=linux/amd64 172.17.0.1:5000/binfmt:latest
docker image save -o binfmt.tar 172.17.0.1:5000/binfmt:amd64 172.17.0.1:5000/binfmt:arm64
tar -xvf binfmt.tar manifest.json
sed -i 's/172\.17\.0\.1:5000/start9\/x_system/g' manifest.json
tar -uf binfmt.tar manifest.json
docker stop registry