FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y git build-essential autoconf curl cmake libfuse-dev pkg-config fuse bc libtool uuid-dev xfslibs-dev libattr1-dev libacl1-dev libaio-dev attr acl quota bsdmainutils dbench psmisc meson cargo rustc

RUN adduser --disabled-password --gecos '' fsgqa

RUN echo 'user_allow_other' >> /etc/fuse.conf

RUN mkdir -p /code && cd /code && git clone https://github.com/fleetfs/fuse-xfstests && cd fuse-xfstests && git checkout c123d014fcca48cf340be78d6712eff80ee4e8d6 && make

RUN cd /code && cargo new backupfs --bin
ADD Cargo.* /code/backupfs
RUN cd /code/backupfs && touch src/lib.rs && RUSTFLAGS=-g cargo build --release --all

RUN cd /code && git clone https://github.com/libfuse/libfuse.git && cd libfuse && git checkout fuse-3.16.2
RUN sed -i.bak '/err += test_mkfifo/d; /err += test_socket/d' /code/libfuse/test/test_syscalls.c
RUN cd /code/libfuse && mkdir build && cd build && meson setup .. && ninja

ADD . /code/backupfs
RUN cd /code/backupfs && touch src/* && RUSTFLAGS=-g cargo build --release --all
RUN cp /code/backupfs/xfstests_common_backupfs /code/fuse-xfstests/common/fuser

ENTRYPOINT /code/backupfs/testing.sh
