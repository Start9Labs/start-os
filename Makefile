OS_ARCH := $(shell echo "${OS_ARCH}")
ARCH := $(shell if [ "$(OS_ARCH)" = "raspberrypi" ]; then echo aarch64; else echo $(OS_ARCH) | sed 's/-nonfree$$//g'; fi)
ENVIRONMENT_FILE = $(shell ./check-environment.sh)
GIT_HASH_FILE = $(shell ./check-git-hash.sh)
VERSION_FILE = $(shell ./check-version.sh)
EMBASSY_BINS := backend/target/$(ARCH)-unknown-linux-gnu/release/startbox libs/target/aarch64-unknown-linux-musl/release/embassy_container_init libs/target/x86_64-unknown-linux-musl/release/embassy_container_init
EMBASSY_UIS := frontend/dist/raw/ui frontend/dist/raw/setup-wizard frontend/dist/raw/diagnostic-ui frontend/dist/raw/install-wizard
BUILD_SRC := $(shell find build)
EMBASSY_SRC := backend/startd.service $(BUILD_SRC)
COMPAT_SRC := $(shell find system-images/compat/ -not -path 'system-images/compat/target/*' -and -not -name *.tar -and -not -name target)
UTILS_SRC := $(shell find system-images/utils/ -not -name *.tar)
BINFMT_SRC := $(shell find system-images/binfmt/ -not -name *.tar)
BACKEND_SRC := $(shell find backend/src) $(shell find backend/migrations) $(shell find patch-db/*/src) $(shell find libs/*/src) libs/*/Cargo.toml backend/Cargo.toml backend/Cargo.lock frontend/dist/static
FRONTEND_SHARED_SRC := $(shell find frontend/projects/shared) $(shell ls -p frontend/ | grep -v / | sed 's/^/frontend\//g') frontend/package.json frontend/node_modules frontend/config.json patch-db/client/dist frontend/patchdb-ui-seed.json
FRONTEND_UI_SRC := $(shell find frontend/projects/ui)
FRONTEND_SETUP_WIZARD_SRC := $(shell find frontend/projects/setup-wizard)
FRONTEND_DIAGNOSTIC_UI_SRC := $(shell find frontend/projects/diagnostic-ui)
FRONTEND_INSTALL_WIZARD_SRC := $(shell find frontend/projects/install-wizard)
PATCH_DB_CLIENT_SRC := $(shell find patch-db/client -not -path patch-db/client/dist -and -not -path patch-db/client/node_modules)
GZIP_BIN := $(shell which pigz || which gzip)
TAR_BIN := $(shell which gtar || which tar)
ALL_TARGETS := $(EMBASSY_BINS) system-images/compat/docker-images/$(ARCH).tar system-images/utils/docker-images/$(ARCH).tar system-images/binfmt/docker-images/$(ARCH).tar $(EMBASSY_SRC) $(shell if [ "$(OS_ARCH)" = "raspberrypi" ]; then echo cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep; fi) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE)

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

.PHONY: all gzip install clean format sdk snapshots frontends ui backend reflash startos_raspberrypi.img sudo wormhole

all: $(ALL_TARGETS)

sudo:
	sudo true

clean:
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

startos_raspberrypi.img: $(BUILD_SRC) startos.raspberrypi.squashfs $(VERSION_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep | sudo
	./build/raspberrypi/make-image.sh

# For creating os images. DO NOT USE
install: $(ALL_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,backend/target/$(ARCH)-unknown-linux-gnu/release/startbox,$(DESTDIR)/usr/bin/startbox)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/startd)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-cli)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-sdk)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/avahi-alias)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/embassy-cli)
	if [ "$(OS_ARCH)" = "raspberrypi" ]; then $(call cp,cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep,$(DESTDIR)/usr/bin/pi-beep); fi
	
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

update-overlay:
	@echo "\033[33m!!! THIS WILL ONLY REFLASH YOUR DEVICE IN MEMORY !!!\033[0m"
	@echo "\033[33mALL CHANGES WILL BE REVERTED IF YOU RESTART THE DEVICE\033[0m"
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	@if [ "`ssh $(REMOTE) 'cat /usr/lib/embassy/VERSION.txt'`" != "`cat ./VERSION.txt`" ]; then >&2 echo "StartOS requires migrations: update-overlay is unavailable." && false; fi
	$(call ssh,"sudo systemctl stop startd")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) OS_ARCH=$(OS_ARCH)
	$(call ssh,"sudo systemctl start startd")

wormhole: backend/target/$(ARCH)-unknown-linux-gnu/release/startbox
	@wormhole send backend/target/$(ARCH)-unknown-linux-gnu/release/startbox 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/embassy/scripts/chroot-and-upgrade \"cd /usr/bin && rm startbox && wormhole receive --accept-file %s && chmod +x startbox\"\n", $$3 }'

update:
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,"sudo rsync -a --delete --force --info=progress2 /media/embassy/embassyfs/current/ /media/embassy/next/")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/embassy/next OS_ARCH=$(OS_ARCH)
	$(call ssh,"sudo touch /media/embassy/config/upgrade && sudo sync && sudo reboot")

emulate-reflash:
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,"sudo rsync -a --delete --force --info=progress2 /media/embassy/embassyfs/current/ /media/embassy/next/")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/embassy/next OS_ARCH=$(OS_ARCH)
	$(call ssh,"sudo touch /media/embassy/config/upgrade && sudo rm -f /media/embassy/config/disk.guid && sudo sync && sudo reboot")

system-images/compat/docker-images/aarch64.tar system-images/compat/docker-images/x86_64.tar: $(COMPAT_SRC)
	cd system-images/compat && make

system-images/utils/docker-images/aarch64.tar system-images/utils/docker-images/x86_64.tar: $(UTILS_SRC)
	cd system-images/utils && make

system-images/binfmt/docker-images/aarch64.tar system-images/binfmt/docker-images/x86_64.tar: $(BINFMT_SRC)
	cd system-images/binfmt && make

snapshots: libs/snapshot_creator/Cargo.toml
	cd libs/  && ./build-v8-snapshot.sh
	cd libs/  && ./build-arm-v8-snapshot.sh

$(EMBASSY_BINS): $(BACKEND_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) frontend/patchdb-ui-seed.json
	cd backend && ARCH=$(ARCH) ./build-prod.sh
	touch $(EMBASSY_BINS)

frontend/node_modules: frontend/package.json
	npm --prefix frontend ci

frontend/dist/raw/ui: $(FRONTEND_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:ui

frontend/dist/raw/setup-wizard: $(FRONTEND_SETUP_WIZARD_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:setup

frontend/dist/raw/diagnostic-ui: $(FRONTEND_DIAGNOSTIC_UI_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:dui

frontend/dist/raw/install-wizard: $(FRONTEND_INSTALL_WIZARD_SRC) $(FRONTEND_SHARED_SRC) $(ENVIRONMENT_FILE)
	npm --prefix frontend run build:install-wiz

frontend/dist/static: $(EMBASSY_UIS)
	./compress-uis.sh

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
ui: frontend/dist/raw/ui

# used by github actions
backend: $(EMBASSY_BINS)

cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep:
	ARCH=aarch64 ./build-cargo-dep.sh pi-beep