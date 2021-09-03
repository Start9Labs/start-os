 COMPAT_SRC := $(shell find ./src) Cargo.toml Cargo.lock

.DELETE_ON_ERROR:

all: compat.tar

compat.tar: Dockerfile build
	DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/x_system/compat --platform=linux/arm64 -o type=docker,dest=compat.tar .

build: $(COMPAT_SRC)
	./build.sh
