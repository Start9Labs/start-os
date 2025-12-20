FROM debian:trixie

RUN apt-get update && \
    apt-get install -y \
    ca-certificates \
    curl \
    gpg \
    build-essential \
    sed \
    grep \
    gawk \
    jq \
    gzip \
    brotli \
    squashfs-tools \
    git \
    debspawn \
    rsync \
    b3sum \
    sudo \
    nodejs

RUN git config --global --add safe.directory /root/start-os

RUN mkdir -p /root/start-os
WORKDIR /root/start-os