ARG SUITE=trixie

FROM debian:${SUITE}

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -yq \
    live-build \
    procps \
    binfmt-support \
    qemu-utils \
    qemu-user-static \
    xorriso \
    isolinux \
    ca-certificates \
    curl \
    wget \
    gpg \
    git \
    fdisk \
    dosfstools \
    e2fsprogs \
    squashfs-tools \
    rsync \
    b3sum \
    dpkg-dev
RUN echo 'retry_connrefused = on' > /etc/wgetrc && \
    echo 'tries = 100' >> /etc/wgetrc

WORKDIR /root