ARCH = aarch64
ENVIRONMENT_FILE := $(shell ./check-environment.sh)
GIT_HASH_FILE := $(shell ./check-git-hash.sh)
EMBASSY_BINS := backend/target/$(ARCH)-unknown-linux-gnu/release/embassyd backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-init backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-cli backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-sdk backend/target/$(ARCH)-unknown-linux-gnu/release/avahi-alias
EMBASSY_UIS := frontend/dist/ui frontend/dist/setup-wizard frontend/dist/diagnostic-ui
EMBASSY_SRC := raspios.img product_key.txt $(EMBASSY_BINS) backend/embassyd.service backend/embassy-init.service $(EMBASSY_UIS) $(shell find build)
COMPAT_SRC := $(shell find system-images/compat/ -not -path 'system-images/compat/target/*' -and -not -name compat.tar -and -not -name target)
UTILS_SRC := $(shell find system-images/utils/ -not -name utils.tar)
BINFMT_SRC := $(shell find system-images/binfmt/ -not -name binfmt.tar)
BACKEND_SRC := $(shell find backend/src) $(shell find backend/migrations) $(shell find patch-db/*/src) backend/Cargo.toml backend/Cargo.lock
FRONTEND_SHARED_SRC := $(shell find frontend/projects/shared) $(shell find frontend/assets) $(shell ls -p frontend/ | grep -v / | sed 's/^/frontend\//g') frontend/node_modules frontend/config.json patch-db/client/dist frontend/patchdb-ui-seed.json
FRONTEND_UI_SRC := $(shell find frontend/projects/ui)
FRONTEND_SETUP_WIZARD_SRC := $(shell find frontend/projects/setup-wizard)
FRONTEND_DIAGNOSTIC_UI_SRC := $(shell find frontend/projects/diagnostic-ui)
PATCH_DB_CLIENT_SRC := $(shell find patch-db/client -not -path patch-db/client/dist)
GZIP_BIN := $(shell which pigz || which gzip)
$(shell sudo true)

.DELETE_ON_ERROR:

.PHONY: all gzip clean format sdk snapshots frontends ui backend
all: eos.img

gzip: eos.tar.gz

eos.tar.gz: eos.img
	tar --format=posix -cS -f- eos.img | $(GZIP_BIN) > eos.tar.gz

clean:
	rm -f eos.img
	rm -f ubuntu.img
	rm -f product_key.txt
	rm -f system-images/**/*.tar
	sudo rm -f $(EMBASSY_BINS)
	rm -f frontend/config.json
	rm -rf frontend/node_modules
	rm -rf frontend/dist
	rm -rf patch-db/client/node_modules
	rm -rf patch-db/client/dist
	sudo rm -rf cargo-deps

format:
	cd backend && cargo +nightly fmt
	cd libs && cargo +nightly fmt

sdk: 
	cd backend/ && ./install-sdk.sh

eos.img: $(EMBASSY_SRC) system-images/compat/compat.tar system-images/utils/utils.tar system-images/binfmt/binfmt.tar cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)
	! test -f eos.img || rm eos.img
	if [ "$(NO_KEY)" = "1" ]; then NO_KEY=1 ./build/make-image.sh; else ./build/make-image.sh; fi

system-images/compat/compat.tar: $(COMPAT_SRC)
	cd system-images/compat && make

system-images/utils/utils.tar: $(UTILS_SRC)
	cd system-images/utils && make

system-images/binfmt/binfmt.tar: $(BINFMT_SRC)
	cd system-images/binfmt && make

raspios.img:
	wget --continue https://downloads.raspberrypi.org/raspios_lite_arm64/images/raspios_lite_arm64-2022-01-28/2022-01-28-raspios-bullseye-arm64-lite.zip
	unzip 2022-01-28-raspios-bullseye-arm64-lite.zip
	mv 2022-01-28-raspios-bullseye-arm64-lite.img raspios.img

product_key.txt:
	$(shell which echo) -n "X" > product_key.txt
	cat /dev/urandom | base32 | head -c11 | tr '[:upper:]' '[:lower:]' >> product_key.txt
	if [ "$(KEY)" != "" ]; then $(shell which echo) -n "$(KEY)" > product_key.txt; fi
	echo >> product_key.txt

snapshots: libs/snapshot-creator/Cargo.toml
	cd libs/  && ./build-v8-snapshot.sh
	cd libs/  && ./build-arm-v8-snapshot.sh

$(EMBASSY_BINS): $(BACKEND_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) frontend/patchdb-ui-seed.json
	cd backend && ./build-prod.sh
	touch $(EMBASSY_BINS)

frontend/node_modules: frontend/package.json
	npm --prefix frontend ci

frontend/dist/ui: $(FRONTEND_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:ui

frontend/dist/setup-wizard: $(FRONTEND_SETUP_WIZARD_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:setup

frontend/dist/diagnostic-ui: $(FRONTEND_DIAGNOSTIC_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:dui

frontend/config.json: $(GIT_HASH_FILE) frontend/config-sample.json
	jq '.useMocks = false' frontend/config-sample.json > frontend/config.json
	npm --prefix frontend run-script build-config

frontend/patchdb-ui-seed.json: frontend/package.json
	jq '."ack-welcome" = "$(shell yq '.version' frontend/package.json)"' frontend/patchdb-ui-seed.json > ui-seed.tmp
	mv ui-seed.tmp frontend/patchdb-ui-seed.json

patch-db/client/node_modules: patch-db/client/package.json
	npm --prefix patch-db/client ci

patch-db/client/dist: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules
	! test -d patch-db/client/dist || rm -rf patch-db/client/dist
	npm --prefix frontend run build:deps

# used by github actions
backend-$(ARCH).tar: $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(EMBASSY_BINS)
	tar -cvf $@ $^

# this is a convenience step to build all frontends - it is not referenced elsewhere in this file
frontends: $(EMBASSY_UIS) 

# this is a convenience step to build the UI
ui: frontend/dist/ui

# used by github actions
backend: $(EMBASSY_BINS)

cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast:
	./build-cargo-dep.sh nc-broadcast
