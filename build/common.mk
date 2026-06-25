ls-files = $(shell git ls-files --cached --others --exclude-standard $1) 
PROFILE = release

PLATFORM_FILE := $(shell ./build/env/check-platform.sh)
ENVIRONMENT_FILE := $(shell ./build/env/check-environment.sh)
GIT_HASH_FILE := $(shell ./build/env/check-git-hash.sh)
VERSION_FILE := $(shell ./build/env/check-version.sh)
BASENAME := $(shell PROJECT=startos ./build/env/basename.sh)
PLATFORM := $(shell if [ -f $(PLATFORM_FILE) ]; then cat $(PLATFORM_FILE); else echo unknown; fi)
ARCH := $(shell if [ "$(PLATFORM)" = "raspberrypi" ]; then echo aarch64; elif [ "$(PLATFORM)" = "rockchip64" ]; then echo aarch64; else echo $(PLATFORM) | sed 's/-nonfree$$//g; s/-nvidia$$//g'; fi)
RUST_ARCH := $(shell if [ "$(ARCH)" = "riscv64" ]; then echo riscv64gc; else echo $(ARCH); fi)
REGISTRY_BASENAME := $(shell PROJECT=start-registry PLATFORM=$(ARCH) ./build/env/basename.sh)
TUNNEL_BASENAME := $(shell PROJECT=start-tunnel PLATFORM=$(ARCH) ./build/env/basename.sh)
IMAGE_TYPE=$(shell if [ "$(PLATFORM)" = raspberrypi ]; then echo img; else echo iso; fi)
WEB_UIS := start-os/web/dist/raw/ui/index.html start-os/web/dist/raw/setup-wizard/index.html
COMPRESSED_WEB_UIS := start-os/web/dist/static/ui/index.html start-os/web/dist/static/setup-wizard/index.html
FIRMWARE_ROMS := build/lib/firmware/$(PLATFORM) $(shell jq --raw-output '.[] | select(.platform[] | contains("$(PLATFORM)")) | "./build/lib/firmware/$(PLATFORM)/" + .id + ".rom.gz"' build/lib/firmware.json)
BUILD_SRC := $(call ls-files, build/lib) build/lib/depends build/lib/conflicts $(FIRMWARE_ROMS) build/lib/migration-images/.done
IMAGE_RECIPE_SRC := $(call ls-files, build/image-recipe/)
STARTD_SRC := start-os/startd.service start-os/services.slice start-os/startos-shutdown.service start-os/startos-restart.service $(BUILD_SRC)
CORE_SRC := $(call ls-files, shared/crates/start-core) $(shell git ls-files --recurse-submodules vendor/patch-db) $(GIT_HASH_FILE)
WEB_SHARED_SRC := $(call ls-files, shared/web/shared) $(call ls-files, shared/web/marketplace) $(shell ls -p shared/web/ | grep -v / | sed 's|^|shared/web/|g') package.json angular.json tsconfig.json tsconfig.lib.json node_modules/.package-lock.json config.json vendor/patch-db/client/dist/index.js start-sdk/baseDist/package.json start-os/web/patchdb-ui-seed.json start-sdk/dist/package.json
WEB_UI_SRC := $(call ls-files, start-os/web/ui)
WEB_SETUP_WIZARD_SRC := $(call ls-files, start-os/web/setup-wizard)
WEB_START_TUNNEL_SRC := $(call ls-files, start-tunnel/web)
PATCH_DB_CLIENT_SRC := $(shell git ls-files --recurse-submodules vendor/patch-db/client)
GZIP_BIN := $(shell which pigz || which gzip)
TAR_BIN := $(shell which gtar || which tar)
COMPILED_TARGETS := target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox target/$(RUST_ARCH)-unknown-linux-musl/release/start-container start-os/container-runtime/rootfs.$(ARCH).squashfs
STARTOS_TARGETS := $(STARTD_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE) $(COMPILED_TARGETS) target/$(RUST_ARCH)-unknown-linux-musl/release/startos-backup-fs $(PLATFORM_FILE) \
	$(shell if [ "$(PLATFORM)" = "raspberrypi" ]; then \
		echo target/aarch64-unknown-linux-musl/release/pi-beep; \
	fi) \
	$(shell /bin/bash -c 'if [[ "${ENVIRONMENT}" =~ (^|-)unstable($$|-) ]]; then \
		echo target/$(RUST_ARCH)-unknown-linux-musl/release/flamegraph; \
	fi') \
	$(shell /bin/bash -c 'if [[ "${ENVIRONMENT}" =~ (^|-)console($$|-) ]]; then \
		echo target/$(RUST_ARCH)-unknown-linux-musl/release/tokio-console; \
	fi')
REGISTRY_TARGETS := target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox start-registry/start-registryd.service
TUNNEL_TARGETS := target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox start-tunnel/start-tunneld.service

ifeq ($(REMOTE),)
	mkdir = mkdir -p $1
	rm = rm -rf $1
	cp = cp -r $1 $2
	ln = ln -sf $1 $2
else
	ifeq ($(SSHPASS),)
		ssh = ssh $(REMOTE) $1
	else 
		ssh = sshpass -p $(SSHPASS) ssh $(REMOTE) $1
	endif
	mkdir = $(call ssh,'sudo mkdir -p $1')
	rm  = $(call ssh,'sudo rm -rf $1')
	ln = $(call ssh,'sudo ln -sf $1 $2')
define cp
	$(TAR_BIN) --transform "s|^$1|x|" -czv -f- $1 | $(call ssh,"sudo tar --transform 's|^x|$2|' -xzv -f- -C /")
endef
endif

.DELETE_ON_ERROR:

# --- vendored patch-db TS client (consumed by web) ---
vendor/patch-db/client/node_modules/.package-lock.json: vendor/patch-db/client/package.json
	npm --prefix vendor/patch-db/client ci
	touch vendor/patch-db/client/node_modules/.package-lock.json

vendor/patch-db/client/dist/index.js: $(PATCH_DB_CLIENT_SRC) vendor/patch-db/client/node_modules/.package-lock.json
	rm -rf vendor/patch-db/client/dist
	npm --prefix vendor/patch-db/client run build
	touch vendor/patch-db/client/dist/index.js

# --- external cargo tools bundled into the OS image ---
target/aarch64-unknown-linux-musl/release/pi-beep: ./build/build-cargo-dep.sh
	ARCH=aarch64 ./build/build-cargo-dep.sh pi-beep

target/$(RUST_ARCH)-unknown-linux-musl/release/tokio-console: ./build/build-cargo-dep.sh
	ARCH=$(ARCH) ./build/build-cargo-dep.sh tokio-console
	touch $@

target/$(RUST_ARCH)-unknown-linux-musl/release/flamegraph: ./build/build-cargo-dep.sh
	ARCH=$(ARCH) ./build/build-cargo-dep.sh flamegraph
	touch $@
