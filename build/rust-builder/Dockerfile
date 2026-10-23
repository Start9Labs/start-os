# syntax=docker/dockerfile:1
ARG BASE_IMAGE=start9/cargo-zigbuild@sha256:d33531387343121af65414661d9160d65e4ca678708d58ce22b1a359399c931e

FROM ${BASE_IMAGE} AS builder
WORKDIR /build/cargo-zigbuild

ADD --checksum=sha256:62239c71c4cb36c47fe000efebfcf6fe63dc7c0feb2346434e950b124220b2e8 https://codeload.github.com/rust-cross/cargo-zigbuild/tar.gz/2ffd80658c96b5ff971085dd509e58e5b441f1c8 /tmp/cargo-zigbuild.tar.gz
RUN tar -xzf /tmp/cargo-zigbuild.tar.gz --strip-components=1 -C .
COPY rva23.patch /tmp/rva23.patch
RUN patch --fuzz=0 -p1 < /tmp/rva23.patch
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    --mount=type=cache,target=/build/cargo-zigbuild/target \
    cargo test --locked --lib && \
    cargo build --locked --release --bin cargo-zigbuild && \
    install -m 755 target/release/cargo-zigbuild /usr/local/bin/cargo-zigbuild-start9

FROM ${BASE_IMAGE}
ARG SOURCE_REVISION
LABEL org.opencontainers.image.source="https://github.com/Start9Labs/start-technologies" \
    org.opencontainers.image.revision="${SOURCE_REVISION}" \
    org.opencontainers.image.description="Start9 Rust cross-compilation environment"
COPY --from=builder /usr/local/bin/cargo-zigbuild-start9 /usr/local/cargo/bin/cargo-zigbuild
COPY --from=builder /build/cargo-zigbuild/LICENSE /usr/local/share/licenses/cargo-zigbuild/LICENSE
COPY verify.sh /usr/local/libexec/start9/verify-rust-builder
