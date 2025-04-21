FROM debian:bookworm

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
    qemu-user-static \
    binfmt-support \
    squashfs-tools \
    git \
    debspawn \
    rsync \
    b3sum \
    fuse-overlayfs \
    sudo \
    systemd \
    systemd-container \
    systemd-sysv \
    dbus \
    dbus-user-session

RUN systemctl mask \
    systemd-firstboot.service \
    systemd-udevd.service \
    getty@tty1.service \
    console-getty.service

RUN git config --global --add safe.directory /root/start-os

RUN mkdir -p /etc/debspawn && \
    echo "AllowUnsafePermissions=true" > /etc/debspawn/global.toml

ENV NVM_DIR=~/.nvm
ENV NODE_VERSION=20
RUN mkdir -p $NVM_DIR && \
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/master/install.sh | bash && \
    . $NVM_DIR/nvm.sh \
    nvm install $NODE_VERSION && \
    nvm use $NODE_VERSION && \
    nvm alias default $NODE_VERSION && \
    ln -s $(which node) /usr/bin/node && \
    ln -s $(which npm) /usr/bin/npm

RUN mkdir -p /root/start-os
WORKDIR /root/start-os

COPY docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT [ "/docker-entrypoint.sh" ]