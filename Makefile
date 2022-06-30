EMBASSY_BINS := backend/target/aarch64-unknown-linux-gnu/release/embassyd backend/target/aarch64-unknown-linux-gnu/release/embassy-init backend/target/aarch64-unknown-linux-gnu/release/embassy-cli backend/target/aarch64-unknown-linux-gnu/release/embassy-sdk
EMBASSY_UIS := frontend/dist/ui frontend/dist/setup-wizard frontend/dist/diagnostic-ui
EMBASSY_SRC := raspios.img product_key.txt $(EMBASSY_BINS) backend/embassyd.service backend/embassy-init.service $(EMBASSY_UIS) $(shell find build)
COMPAT_SRC := $(shell find system-images/compat/src)
UTILS_SRC := $(shell find system-images/utils/Dockerfile)
BACKEND_SRC := $(shell find backend/src) $(shell find patch-db/*/src) $(shell find rpc-toolkit/*/src) backend/Cargo.toml backend/Cargo.lock
FRONTEND_SRC := $(shell find frontend/projects) $(shell find frontend/assets)
PATCH_DB_CLIENT_SRC = $(shell find patch-db/client -not -path patch-db/client/dist)
GIT_REFS := $(shell find .git/refs/heads)
TMP_FILE := $(shell mktemp)

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

sdk: 
	cd backend/ && ./install-sdk.sh

eos.img: $(EMBASSY_SRC) system-images/compat/compat.tar system-images/utils/utils.tar 
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

$(EMBASSY_BINS): $(BACKEND_SRC) 
	cd backend && ./build-prod.sh

frontend/node_modules: frontend/package.json
	npm --prefix frontend ci

$(EMBASSY_UIS): $(FRONTEND_SRC) frontend/node_modules patch-db/client patch-db/client/dist frontend/config.json
	npm --prefix frontend run build:all

frontend/config.json: .git/HEAD $(GIT_REFS)
	jq '.useMocks = false' frontend/config-sample.json > frontend/config.json
	npm --prefix frontend run-script build-config

patch-db/client/node_modules: patch-db/client/package.json
	npm --prefix patch-db/client install

patch-db/client/dist: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules
	! test -d patch-db/client/dist || rm -rf patch-db/client/dist
	npm --prefix patch-db/client run build

# this is a convenience step to build all frontends - it is not referenced elsewhere in this file
frontends: frontend/node_modules frontend/config.json $(EMBASSY_UIS) 

# this is a convenience step to build the UI
ui: frontend/node_modules frontend/config.json frontend/dist/ui
