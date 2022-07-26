ENVIRONMENT_FILE := $(shell ./check-environment.sh)
GIT_HASH_FILE := $(shell ./check-git-hash.sh)
EMBASSY_BINS := backend/target/aarch64-unknown-linux-gnu/release/embassyd backend/target/aarch64-unknown-linux-gnu/release/embassy-init backend/target/aarch64-unknown-linux-gnu/release/embassy-cli backend/target/aarch64-unknown-linux-gnu/release/embassy-sdk
EMBASSY_UIS := frontend/dist/ui frontend/dist/setup-wizard frontend/dist/diagnostic-ui
EMBASSY_SRC := raspios.img product_key.txt $(EMBASSY_BINS) backend/embassyd.service backend/embassy-init.service $(EMBASSY_UIS) $(shell find build)
COMPAT_SRC := $(shell find system-images/compat/src)
UTILS_SRC := $(shell find system-images/utils/Dockerfile)
BACKEND_SRC := $(shell find backend/src) $(shell find patch-db/*/src) backend/Cargo.toml backend/Cargo.lock
FRONTEND_SHARED_SRC := $(shell find frontend/projects/shared) $(shell find frontend/assets) $(shell ls -p frontend/ | grep -v / | sed 's/^/frontend\//g') frontend/node_modules frontend/config.json patch-db/client/dist
FRONTEND_UI_SRC := $(shell find frontend/projects/ui)
FRONTEND_SETUP_WIZARD_SRC := $(shell find frontend/projects/setup-wizard)
FRONTEND_DIAGNOSTIC_UI_SRC := $(shell find frontend/projects/diagnostic-ui)
PATCH_DB_CLIENT_SRC := $(shell find patch-db/client -not -path patch-db/client/dist)
$(shell sudo true)

.DELETE_ON_ERROR:

all: eos.img

gzip: eos.img
	gzip -k eos.img

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

sdk: 
	cd backend/ && ./install-sdk.sh

eos.img: $(EMBASSY_SRC) system-images/compat/compat.tar system-images/utils/utils.tar cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)
	! test -f eos.img || rm eos.img
	if [ "$(NO_KEY)" = "1" ]; then NO_KEY=1 ./build/make-image.sh; else ./build/make-image.sh; fi

system-images/compat/compat.tar: $(COMPAT_SRC)
	cd system-images/compat && ./build.sh
	cd system-images/compat && DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/x_system/compat --platform=linux/arm64 -o type=docker,dest=compat.tar .

system-images/utils/utils.tar: $(UTILS_SRC)
	cd system-images/utils && DOCKER_CLI_EXPERIMENTAL=enabled docker buildx build --tag start9/x_system/utils --platform=linux/arm64 -o type=docker,dest=utils.tar .

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

$(EMBASSY_BINS): $(BACKEND_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)
	cd backend && ./build-prod.sh
	touch $(EMBASSY_BINS)

frontend/node_modules: frontend/package.json
	npm --prefix frontend ci

frontend/dist/ui: $(FRONTEND_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:ui

frontend/dist/setup-wizard: $(FRONTEND_SETUP_WIZARD_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:setup-wizard

frontend/dist/diagnostic-ui: $(FRONTEND_DIAGNOSTIC_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:diagnostic-ui

frontend/config.json: $(GIT_HASH_FILE) frontend/config-sample.json
	jq '.useMocks = false' frontend/config-sample.json > frontend/config.json
	npm --prefix frontend run-script build-config

patch-db/client/node_modules: patch-db/client/package.json
	npm --prefix patch-db/client ci

patch-db/client/dist: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules
	! test -d patch-db/client/dist || rm -rf patch-db/client/dist
	rm -rf frontend/.angular/cache
	npm --prefix patch-db/client run build

# this is a convenience step to build all frontends - it is not referenced elsewhere in this file
frontends: $(EMBASSY_UIS) 

# this is a convenience step to build the UI
ui: frontend/dist/ui

# this is a convenience step to build the backend
backend: $(EMBASSY_BINS)

cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast:
	./build-cargo-dep.sh nc-broadcast