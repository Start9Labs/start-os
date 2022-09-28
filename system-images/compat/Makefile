 COMPAT_SRC := $(shell find ./src) Cargo.toml Cargo.lock

.DELETE_ON_ERROR:

all: compat.tar

compat.tar: Dockerfile target/aarch64-unknown-linux-musl/release/compat
	DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/x_system/compat --platform=linux/arm64 -o type=docker,dest=compat.tar .

target/aarch64-unknown-linux-musl/release/compat: $(COMPAT_SRC)
	./build.sh
