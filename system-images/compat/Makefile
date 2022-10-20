 COMPAT_SRC := $(shell find ./src) Cargo.toml Cargo.lock

.DELETE_ON_ERROR:

all: docker-images/aarch64.tar

clean:
	cargo clean
	rm -rf docker-images

docker-images:
	mkdir docker-images

docker-images/aarch64.tar: Dockerfile target/aarch64-unknown-linux-musl/release/compat docker-images
	docker buildx build --tag start9/x_system/compat --platform=linux/arm64 -o type=docker,dest=docker-images/aarch64.tar .

target/aarch64-unknown-linux-musl/release/compat: $(COMPAT_SRC)
	./build.sh
