FROM debian:bookworm-slim

RUN apt update && \
    apt install --no-install-recommends -y squashfs-tools-ng && \
    apt clean && \
    rm -rf /var/lib/apt/lists/*
