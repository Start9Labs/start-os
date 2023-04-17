RASPI_TARGETS := eos_raspberrypi-uninit.img eos_raspberrypi-uninit.tar.gz
OS_ARCH := $(shell if echo $(RASPI_TARGETS) | grep -qw "$(MAKECMDGOALS)"; then echo raspberrypi; elif [ -n "${OS_ARCH}" ]; then echo "${OS_ARCH}"; else uname -m; fi)
ARCH := $(shell if [ "$(OS_ARCH)" = "raspberrypi" ]; then echo aarch64; else echo $(OS_ARCH); fi)
ENVIRONMENT_FILE = $(shell ./check-environment.sh)
GIT_HASH_FILE = $(shell ./check-git-hash.sh)
VERSION_FILE = $(shell ./check-version.sh)
EMBASSY_BINS := backend/target/$(ARCH)-unknown-linux-gnu/release/embassyd backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-init backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-cli backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-sdk backend/target/$(ARCH)-unknown-linux-gnu/release/avahi-alias libs/target/aarch64-unknown-linux-musl/release/embassy_container_init libs/target/x86_64-unknown-linux-musl/release/embassy_container_init
EMBASSY_UIS := frontend/dist/ui frontend/dist/setup-wizard frontend/dist/diagnostic-ui frontend/dist/install-wizard
BUILD_SRC := $(shell find build)
EMBASSY_SRC := backend/embassyd.service backend/embassy-init.service $(EMBASSY_UIS) $(BUILD_SRC)
COMPAT_SRC := $(shell find system-images/compat/ -not -path 'system-images/compat/target/*' -and -not -name *.tar -and -not -name target)
UTILS_SRC := $(shell find system-images/utils/ -not -name *.tar)
BINFMT_SRC := $(shell find system-images/binfmt/ -not -name *.tar)
BACKEND_SRC := $(shell find backend/src) $(shell find backend/migrations) $(shell find patch-db/*/src) $(shell find libs/*/src) libs/*/Cargo.toml backend/Cargo.toml backend/Cargo.lock
FRONTEND_SHARED_SRC := $(shell find frontend/projects/shared) $(shell ls -p frontend/ | grep -v / | sed 's/^/frontend\//g') frontend/package.json frontend/node_modules frontend/config.json patch-db/client/dist frontend/patchdb-ui-seed.json
FRONTEND_UI_SRC := $(shell find frontend/projects/ui)
FRONTEND_SETUP_WIZARD_SRC := $(shell find frontend/projects/setup-wizard)
FRONTEND_DIAGNOSTIC_UI_SRC := $(shell find frontend/projects/diagnostic-ui)
FRONTEND_INSTALL_WIZARD_SRC := $(shell find frontend/projects/install-wizard)
PATCH_DB_CLIENT_SRC := $(shell find patch-db/client -not -path patch-db/client/dist)
GZIP_BIN := $(shell which pigz || which gzip)
ALL_TARGETS := $(EMBASSY_BINS) system-images/compat/docker-images/$(ARCH).tar system-images/utils/docker-images/$(ARCH).tar system-images/binfmt/docker-images/$(ARCH).tar $(EMBASSY_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE)

ifeq ($(REMOTE),)
	mkdir = mkdir -p $1
	rm = rm -rf $1
	cp = cp -r $1 $2
else
	mkdir = ssh $(REMOTE) 'mkdir -p $1'
	rm  = ssh $(REMOTE) 'sudo rm -rf $1'
define cp
	tar --transform "s|^$1|x|" -czv -f- $1 | ssh $(REMOTE) "sudo tar --transform 's|^x|$2|' -xzv -f- -C /"
endef
endif

.DELETE_ON_ERROR:

.PHONY: all gzip install clean format sdk snapshots frontends ui backend reflash eos_raspberrypi.img sudo

all: $(ALL_TARGETS)

sudo:
	sudo true

clean:
	rm -f 2022-01-28-raspios-bullseye-arm64-lite.zip
	rm -f raspios.img
	rm -f eos_raspberrypi-uninit.img
	rm -f eos_raspberrypi-uninit.tar.gz
	rm -f ubuntu.img
	rm -f product_key.txt
	rm -f system-images/**/*.tar
	rm -rf system-images/compat/target
	rm -rf backend/target
	rm -rf frontend/.angular
	rm -f frontend/config.json
	rm -rf frontend/node_modules
	rm -rf frontend/dist
	rm -rf libs/target
	rm -rf patch-db/client/node_modules
	rm -rf patch-db/client/dist
	rm -rf patch-db/target
	rm -rf cargo-deps
	rm ENVIRONMENT.txt
	rm GIT_HASH.txt
	rm VERSION.txt

format:
	cd backend && cargo +nightly fmt
	cd libs && cargo +nightly fmt

sdk:
	cd backend/ && ./install-sdk.sh

eos_raspberrypi-uninit.img: $(ALL_TARGETS) raspios.img cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep | sudo
	! test -f eos_raspberrypi-uninit.img || rm eos_raspberrypi-uninit.img
	./build/raspberry-pi/make-image.sh

lite-upgrade.img: raspios.img cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep $(BUILD_SRC) eos.raspberrypi.squashfs
	! test -f lite-upgrade.img || rm lite-upgrade.img
	./build/raspberry-pi/make-upgrade-image.sh

eos_raspberrypi.img: raspios.img $(BUILD_SRC) eos.raspberrypi.squashfs $(VERSION_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) | sudo
	! test -f eos_raspberrypi.img || rm eos_raspberrypi.img
	./build/raspberry-pi/make-initialized-image.sh

# For creating os images. DO NOT USE
install: $(ALL_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-init,$(DESTDIR)/usr/bin/embassy-init)
	$(call cp,backend/target/$(ARCH)-unknown-linux-gnu/release/embassyd,$(DESTDIR)/usr/bin/embassyd)
	$(call cp,backend/target/$(ARCH)-unknown-linux-gnu/release/embassy-cli,$(DESTDIR)/usr/bin/embassy-cli)
	$(call cp,backend/target/$(ARCH)-unknown-linux-gnu/release/avahi-alias,$(DESTDIR)/usr/bin/avahi-alias)
	
	$(call mkdir,$(DESTDIR)/usr/lib)
	$(call rm,$(DESTDIR)/usr/lib/embassy)
	$(call cp,build/lib,$(DESTDIR)/usr/lib/embassy)

	$(call cp,ENVIRONMENT.txt,$(DESTDIR)/usr/lib/embassy/ENVIRONMENT.txt)
	$(call cp,GIT_HASH.txt,$(DESTDIR)/usr/lib/embassy/GIT_HASH.txt)
	$(call cp,VERSION.txt,$(DESTDIR)/usr/lib/embassy/VERSION.txt)

	$(call mkdir,$(DESTDIR)/usr/lib/embassy/container)
	$(call cp,libs/target/aarch64-unknown-linux-musl/release/embassy_container_init,$(DESTDIR)/usr/lib/embassy/container/embassy_container_init.arm64)
	$(call cp,libs/target/x86_64-unknown-linux-musl/release/embassy_container_init,$(DESTDIR)/usr/lib/embassy/container/embassy_container_init.amd64)

	$(call mkdir,$(DESTDIR)/usr/lib/embassy/system-images)
	$(call cp,system-images/compat/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/embassy/system-images/compat.tar)
	$(call cp,system-images/utils/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/embassy/system-images/utils.tar)
	$(call cp,system-images/binfmt/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/embassy/system-images/binfmt.tar)

	$(call mkdir,$(DESTDIR)/var/www/html)
	$(call cp,frontend/dist/diagnostic-ui,$(DESTDIR)/var/www/html/diagnostic)
	$(call cp,frontend/dist/setup-wizard,$(DESTDIR)/var/www/html/setup)
	$(call cp,frontend/dist/install-wizard,$(DESTDIR)/var/www/html/install)
	$(call cp,frontend/dist/ui,$(DESTDIR)/var/www/html/main)
	$(call cp,index.html,$(DESTDIR)/var/www/html/index.html)

update-overlay:
	@echo "\033[33m!!! THIS WILL ONLY REFLASH YOUR DEVICE IN MEMORY !!!\033[0m"
	@echo "\033[33mALL CHANGES WILL BE REVERTED IF YOU RESTART THE DEVICE\033[0m"
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	@if [ "`ssh $(REMOTE) 'cat /usr/lib/embassy/VERSION.txt'`" != "`cat ./VERSION.txt`" ]; then >&2 echo "Embassy requires migrations: update-overlay is unavailable." && false; fi
	@if ssh $(REMOTE) "pidof embassy-init"; then >&2 echo "Embassy in INIT: update-overlay is unavailable." && false; fi
	ssh $(REMOTE) "sudo systemctl stop embassyd"
	$(MAKE) install REMOTE=$(REMOTE) OS_ARCH=$(OS_ARCH)
	ssh $(REMOTE) "sudo systemctl start embassyd"

update:
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	ssh $(REMOTE) "sudo rsync -a --delete --force --info=progress2 /media/embassy/embassyfs/current/ /media/embassy/next/"
	$(MAKE) install REMOTE=$(REMOTE) DESTDIR=/media/embassy/next OS_ARCH=$(OS_ARCH)
	ssh $(REMOTE) "sudo touch /media/embassy/config/upgrade && sudo sync && sudo reboot"

emulate-reflash:
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	ssh $(REMOTE) "sudo rsync -a --delete --force --info=progress2 /media/embassy/embassyfs/current/ /media/embassy/next/"
	$(MAKE) install REMOTE=$(REMOTE) DESTDIR=/media/embassy/next OS_ARCH=$(OS_ARCH)
	ssh $(REMOTE) "sudo touch /media/embassy/config/upgrade && sudo rm -f /media/embassy/config/disk.guid && sudo sync && sudo reboot"

system-images/compat/docker-images/aarch64.tar system-images/compat/docker-images/x86_64.tar: $(COMPAT_SRC)
	cd system-images/compat && make

system-images/utils/docker-images/aarch64.tar system-images/utils/docker-images/x86_64.tar: $(UTILS_SRC)
	cd system-images/utils && make

system-images/binfmt/docker-images/aarch64.tar system-images/binfmt/docker-images/x86_64.tar: $(BINFMT_SRC)
	cd system-images/binfmt && make

raspios.img:
	wget --continue https://downloads.raspberrypi.org/raspios_lite_arm64/images/raspios_lite_arm64-2022-01-28/2022-01-28-raspios-bullseye-arm64-lite.zip
	unzip 2022-01-28-raspios-bullseye-arm64-lite.zip
	mv 2022-01-28-raspios-bullseye-arm64-lite.img raspios.img

snapshots: libs/snapshot_creator/Cargo.toml
	cd libs/  && ./build-v8-snapshot.sh
	cd libs/  && ./build-arm-v8-snapshot.sh

$(EMBASSY_BINS): $(BACKEND_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) frontend/patchdb-ui-seed.json
	cd backend && ARCH=$(ARCH) ./build-prod.sh
	touch $(EMBASSY_BINS)

frontend/node_modules: frontend/package.json
	npm --prefix frontend ci

frontend/dist/ui: $(FRONTEND_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:ui

frontend/dist/setup-wizard: $(FRONTEND_SETUP_WIZARD_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:setup

frontend/dist/diagnostic-ui: $(FRONTEND_DIAGNOSTIC_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:dui

frontend/dist/install-wizard: $(FRONTEND_INSTALL_WIZARD_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:install-wiz

frontend/config.json: $(GIT_HASH_FILE) frontend/config-sample.json
	jq '.useMocks = false' frontend/config-sample.json > frontend/config.json
	jq '.packageArch = "$(ARCH)"' frontend/config.json > frontend/config.json.tmp
	jq '.osArch = "$(OS_ARCH)"' frontend/config.json.tmp > frontend/config.json
	rm frontend/config.json.tmp
	npm --prefix frontend run-script build-config

frontend/patchdb-ui-seed.json: frontend/package.json
	jq '."ack-welcome" = $(shell yq '.version' frontend/package.json)' frontend/patchdb-ui-seed.json > ui-seed.tmp
	mv ui-seed.tmp frontend/patchdb-ui-seed.json

patch-db/client/node_modules: patch-db/client/package.json
	npm --prefix patch-db/client ci

patch-db/client/dist: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules
	! test -d patch-db/client/dist || rm -rf patch-db/client/dist
	npm --prefix frontend run build:deps

# used by github actions
backend-$(ARCH).tar: $(EMBASSY_BINS)
	tar -cvf $@ $^

# this is a convenience step to build all frontends - it is not referenced elsewhere in this file
frontends: $(EMBASSY_UIS) 

# this is a convenience step to build the UI
ui: frontend/dist/ui

# used by github actions
backend: $(EMBASSY_BINS)

cargo-deps/aarch64-unknown-linux-gnu/release/nc-broadcast:
	./build-cargo-dep.sh nc-broadcast

cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep:
	./build-cargo-dep.sh pi-beep