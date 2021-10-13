EMBASSY_BINS := appmgr/target/aarch64-unknown-linux-gnu/release/embassyd appmgr/target/aarch64-unknown-linux-gnu/release/embassy-init appmgr/target/aarch64-unknown-linux-gnu/release/embassy-cli appmgr/target/aarch64-unknown-linux-gnu/release/embassy-sdk
EMBASSY_UIS := ui/www setup-wizard/www diagnostic-ui/www
EMBASSY_SRC := ubuntu.img product_key.txt $(EMBASSY_BINS) appmgr/embassyd.service appmgr/embassy-init.service $(EMBASSY_UIS) $(shell find build)

APPMGR_SRC := $(shell find appmgr/src) $(shell find patch-db/*/src) $(shell find rpc-toolkit/*/src) appmgr/Cargo.toml appmgr/Cargo.lock
UI_SRC := $(shell find ui/src)
SETUP_WIZARD_SRC := $(shell find setup-wizard/src)
DIAGNOSTIC_UI_SRC := $(shell find diagnostic-ui/src)
PATCH_DB_CLIENT_SRC = $(shell find patch-db/client -not -path patch-db/client/dist)

all: eos.img

clean:
	rm -f eos.img
	rm -f ubuntu.img
	rm -f product_key.txt
	sudo rm -f $(EMBASSY_BINS)
	rm -rf ui/node_modules
	rm -rf ui/www
	rm -rf setup-wizard/node_modules
	rm -rf setup-wizard/www
	rm -rf diagnostic-ui/node_modules
	rm -rf diagnostic-ui/www
	rm -rf patch-db/client/node_modules
	rm -rf patch-db/client/dist

eos.img: $(EMBASSY_SRC)
	! test -f eos.img || rm eos.img
	./build/make-image.sh

ubuntu.img:
	wget -O ubuntu.img.xz https://cdimage.ubuntu.com/releases/21.04/release/ubuntu-21.04-preinstalled-server-arm64+raspi.img.xz
	unxz ubuntu.img.xz

product_key.txt:
	$(shell which echo) -n "X" > product_key.txt
	cat /dev/random | base32 | head -c11 | tr '[:upper:]' '[:lower:]' >> product_key.txt
	echo >> product_key.txt

$(EMBASSY_BINS): $(APPMGR_SRC)
	cd appmgr && ./build-prod.sh

ui/node_modules: ui/package.json
	npm --prefix ui install

ui/www: $(UI_SRC) ui/node_modules patch-db/client/dist ui/config.json
	npm --prefix ui run build-prod

ui/config.json:
	jq '.mocks.enabled = false' ui/config-sample.json > ui/config.json

setup-wizard/node_modules: setup-wizard/package.json
	npm --prefix setup-wizard install

setup-wizard/www: $(SETUP_WIZARD_SRC) setup-wizard/node_modules setup-wizard/config.json
	npm --prefix setup-wizard run build-prod

setup-wizard/config.json:
	jq '.useMocks = false' setup-wizard/config-sample.json > setup-wizard/config.json

diagnostic-ui/node_modules: diagnostic-ui/package.json
	npm --prefix diagnostic-ui install

diagnostic-ui/www: $(DIAGNOSTIC_UI_SRC) diagnostic-ui/node_modules diagnostic-ui/config.json
	npm --prefix diagnostic-ui run build-prod

diagnostic-ui/config.json:
	jq '.useMocks = false' diagnostic-ui/config-sample.json > diagnostic-ui/config.json

patch-db/client/node_modules: patch-db/client/package.json
	npm --prefix patch-db/client install

patch-db/client/dist: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules
	! test -d patch-db/client/dist || rm -rf patch-db/client/dist
	npm --prefix patch-db/client run build

ui: $(EMBASSY_UIS)

