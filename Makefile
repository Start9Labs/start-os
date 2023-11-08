PLATFORM_FILE := $(shell ./check-platform.sh)
ENVIRONMENT_FILE := $(shell ./check-environment.sh)
GIT_HASH_FILE := $(shell ./check-git-hash.sh)
VERSION_FILE := $(shell ./check-version.sh)
BASENAME := $(shell ./basename.sh)
PLATFORM := $(shell if [ -f ./PLATFORM.txt ]; then cat ./PLATFORM.txt; else echo unknown; fi)
ARCH := $(shell if [ "$(PLATFORM)" = "raspberrypi" ]; then echo aarch64; else echo $(PLATFORM) | sed 's/-nonfree$$//g'; fi)
IMAGE_TYPE=$(shell if [ "$(PLATFORM)" = raspberrypi ]; then echo img; else echo iso; fi)
EMBASSY_BINS := backend/target/$(ARCH)-unknown-linux-gnu/release/startbox libs/target/aarch64-unknown-linux-musl/release/embassy_container_init libs/target/x86_64-unknown-linux-musl/release/embassy_container_init
EMBASSY_UIS := frontend/dist/raw/ui frontend/dist/raw/setup-wizard frontend/dist/raw/diagnostic-ui frontend/dist/raw/install-wizard
BUILD_SRC := $(shell git ls-files build) build/lib/depends build/lib/conflicts
DEBIAN_SRC := $(shell git ls-files debian/)
IMAGE_RECIPE_SRC := $(shell git ls-files image-recipe/)
EMBASSY_SRC := backend/startd.service $(BUILD_SRC)
COMPAT_SRC := $(shell git ls-files system-images/compat/)
UTILS_SRC := $(shell git ls-files system-images/utils/)
BINFMT_SRC := $(shell git ls-files system-images/binfmt/)
BACKEND_SRC := $(shell git ls-files backend) $(shell git ls-files --recurse-submodules patch-db) $(shell git ls-files libs) frontend/dist/static
FRONTEND_SHARED_SRC := $(shell git ls-files frontend/projects/shared) $(shell ls -p frontend/ | grep -v / | sed 's/^/frontend\//g') frontend/node_modules frontend/config.json patch-db/client/dist frontend/patchdb-ui-seed.json
FRONTEND_UI_SRC := $(shell git ls-files frontend/projects/ui)
FRONTEND_SETUP_WIZARD_SRC := $(shell git ls-files frontend/projects/setup-wizard)
FRONTEND_DIAGNOSTIC_UI_SRC := $(shell git ls-files frontend/projects/diagnostic-ui)
FRONTEND_INSTALL_WIZARD_SRC := $(shell git ls-files frontend/projects/install-wizard)
PATCH_DB_CLIENT_SRC := $(shell git ls-files --recurse-submodules patch-db/client)
GZIP_BIN := $(shell which pigz || which gzip)
TAR_BIN := $(shell which gtar || which tar)
COMPILED_TARGETS := $(EMBASSY_BINS) system-images/compat/docker-images/$(ARCH).tar system-images/utils/docker-images/$(ARCH).tar system-images/binfmt/docker-images/$(ARCH).tar
ALL_TARGETS := $(EMBASSY_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE) $(COMPILED_TARGETS) $(shell if [ "$(PLATFORM)" = "raspberrypi" ]; then echo cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep; fi)  $(shell /bin/bash -c 'if [[ "${ENVIRONMENT}" =~ (^|-)unstable($$|-) ]]; then echo cargo-deps/$(ARCH)-unknown-linux-gnu/release/tokio-console; fi') $(PLATFORM_FILE)

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

.PHONY: all metadata install clean format sdk snapshots frontends ui backend reflash deb $(IMAGE_TYPE) squashfs sudo wormhole

all: $(ALL_TARGETS)

metadata: $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)

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
	rm -rf dpkg-workdir
	rm -rf image-recipe/deb
	rm -rf results
	rm -f ENVIRONMENT.txt
	rm -f PLATFORM.txt
	rm -f GIT_HASH.txt
	rm -f VERSION.txt

format:
	cd backend && cargo +nightly fmt
	cd libs && cargo +nightly fmt

sdk:
	cd backend/ && ./install-sdk.sh

deb: results/$(BASENAME).deb

debian/control: build/lib/depends build/lib/conflicts
	./debuild/control.sh

results/$(BASENAME).deb: dpkg-build.sh $(DEBIAN_SRC) $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)
	PLATFORM=$(PLATFORM) ./dpkg-build.sh

$(IMAGE_TYPE): results/$(BASENAME).$(IMAGE_TYPE)

squashfs: results/$(BASENAME).squashfs

results/$(BASENAME).$(IMAGE_TYPE) results/$(BASENAME).squashfs: $(IMAGE_RECIPE_SRC) results/$(BASENAME).deb
	./image-recipe/run-local-build.sh "results/$(BASENAME).deb"

# For creating os images. DO NOT USE
install: $(ALL_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,backend/target/$(ARCH)-unknown-linux-gnu/release/startbox,$(DESTDIR)/usr/bin/startbox)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/startd)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-cli)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-sdk)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-deno)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/avahi-alias)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/embassy-cli)
	if [ "$(PLATFORM)" = "raspberrypi" ]; then $(call cp,cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep,$(DESTDIR)/usr/bin/pi-beep); fi
	if /bin/bash -c '[[ "${ENVIRONMENT}" =~ (^|-)unstable($$|-) ]]'; then $(call cp,cargo-deps/$(ARCH)-unknown-linux-gnu/release/tokio-console,$(DESTDIR)/usr/bin/tokio-console); fi
	
	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,backend/startd.service,$(DESTDIR)/lib/systemd/system/startd.service)

	$(call mkdir,$(DESTDIR)/usr/lib)
	$(call rm,$(DESTDIR)/usr/lib/startos)
	$(call cp,build/lib,$(DESTDIR)/usr/lib/startos)

	$(call cp,PLATFORM.txt,$(DESTDIR)/usr/lib/startos/PLATFORM.txt)
	$(call cp,ENVIRONMENT.txt,$(DESTDIR)/usr/lib/startos/ENVIRONMENT.txt)
	$(call cp,GIT_HASH.txt,$(DESTDIR)/usr/lib/startos/GIT_HASH.txt)
	$(call cp,VERSION.txt,$(DESTDIR)/usr/lib/startos/VERSION.txt)

	$(call mkdir,$(DESTDIR)/usr/lib/startos/container)
	$(call cp,libs/target/aarch64-unknown-linux-musl/release/embassy_container_init,$(DESTDIR)/usr/lib/startos/container/embassy_container_init.arm64)
	$(call cp,libs/target/x86_64-unknown-linux-musl/release/embassy_container_init,$(DESTDIR)/usr/lib/startos/container/embassy_container_init.amd64)

	$(call mkdir,$(DESTDIR)/usr/lib/startos/system-images)
	$(call cp,system-images/compat/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/startos/system-images/compat.tar)
	$(call cp,system-images/utils/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/startos/system-images/utils.tar)
	$(call cp,system-images/binfmt/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/startos/system-images/binfmt.tar)

update-overlay: $(ALL_TARGETS)
	@echo "\033[33m!!! THIS WILL ONLY REFLASH YOUR DEVICE IN MEMORY !!!\033[0m"
	@echo "\033[33mALL CHANGES WILL BE REVERTED IF YOU RESTART THE DEVICE\033[0m"
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	@if [ "`ssh $(REMOTE) 'cat /usr/lib/startos/VERSION.txt'`" != "`cat ./VERSION.txt`" ]; then >&2 echo "StartOS requires migrations: update-overlay is unavailable." && false; fi
	$(call ssh,"sudo systemctl stop startd")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) PLATFORM=$(PLATFORM)
	$(call ssh,"sudo systemctl start startd")

wormhole: backend/target/$(ARCH)-unknown-linux-gnu/release/startbox
	@wormhole send backend/target/$(ARCH)-unknown-linux-gnu/release/startbox 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade \"cd /usr/bin && rm startbox && wormhole receive --accept-file %s && chmod +x startbox\"\n", $$3 }'

update: $(ALL_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,"sudo rsync -a --delete --force --info=progress2 /media/embassy/embassyfs/current/ /media/embassy/next/")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/embassy/next PLATFORM=$(PLATFORM)
	$(call ssh,'sudo NO_SYNC=1 /media/embassy/next/usr/lib/startos/scripts/chroot-and-upgrade "apt-get install -y $(shell cat ./build/lib/depends)"')

emulate-reflash: $(ALL_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,"sudo rsync -a --delete --force --info=progress2 /media/embassy/embassyfs/current/ /media/embassy/next/")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/embassy/next PLATFORM=$(PLATFORM)
	$(call ssh,"sudo touch /media/embassy/config/upgrade && sudo rm -f /media/embassy/config/disk.guid && sudo sync && sudo reboot")

upload-ota: results/$(BASENAME).squashfs
	TARGET=$(TARGET) KEY=$(KEY) ./upload-ota.sh

build/lib/depends build/lib/conflicts: build/dpkg-deps/*
	build/dpkg-deps/generate.sh

system-images/compat/docker-images/$(ARCH).tar: $(COMPAT_SRC) backend/Cargo.lock
	cd system-images/compat && make docker-images/$(ARCH).tar && touch docker-images/$(ARCH).tar

system-images/utils/docker-images/$(ARCH).tar: $(UTILS_SRC)
	cd system-images/utils && make docker-images/$(ARCH).tar && touch docker-images/$(ARCH).tar

system-images/binfmt/docker-images/$(ARCH).tar: $(BINFMT_SRC)
	cd system-images/binfmt && make docker-images/$(ARCH).tar && touch docker-images/$(ARCH).tar

snapshots: libs/snapshot_creator/Cargo.toml
	cd libs/  && ./build-v8-snapshot.sh
	cd libs/  && ./build-arm-v8-snapshot.sh

$(EMBASSY_BINS): $(BACKEND_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) frontend/patchdb-ui-seed.json | sudo
	cd backend && ARCH=$(ARCH) ./build-prod.sh
	touch $(EMBASSY_BINS)

frontend/node_modules: frontend/package.json
	npm --prefix frontend ci

frontend/dist/raw/ui: $(FRONTEND_UI_SRC) $(FRONTEND_SHARED_SRC)
	npm --prefix frontend run build:ui

frontend/dist/raw/setup-wizard: $(FRONTEND_SETUP_WIZARD_SRC) $(FRONTEND_SHARED_SRC)
	npm --prefix frontend run build:setup

frontend/dist/raw/diagnostic-ui: $(FRONTEND_DIAGNOSTIC_UI_SRC) $(FRONTEND_SHARED_SRC)
	npm --prefix frontend run build:dui

frontend/dist/raw/install-wizard: $(FRONTEND_INSTALL_WIZARD_SRC) $(FRONTEND_SHARED_SRC)
	npm --prefix frontend run build:install-wiz

frontend/dist/static: $(EMBASSY_UIS) $(ENVIRONMENT_FILE)
	./compress-uis.sh

frontend/config.json: $(GIT_HASH_FILE) frontend/config-sample.json
	jq '.useMocks = false' frontend/config-sample.json | jq '.gitHash = "$(shell cat GIT_HASH.txt)"' > frontend/config.json

frontend/patchdb-ui-seed.json: frontend/package.json
	jq '."ack-welcome" = $(shell jq '.version' frontend/package.json)' frontend/patchdb-ui-seed.json > ui-seed.tmp
	mv ui-seed.tmp frontend/patchdb-ui-seed.json

patch-db/client/node_modules: patch-db/client/package.json
	npm --prefix patch-db/client ci

patch-db/client/dist: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules
	! test -d patch-db/client/dist || rm -rf patch-db/client/dist
	npm --prefix frontend run build:deps

# used by github actions
compiled-$(ARCH).tar: $(COMPILED_TARGETS) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE)
	tar -cvf $@ $^

# this is a convenience step to build all frontends - it is not referenced elsewhere in this file
frontends: $(EMBASSY_UIS) 

# this is a convenience step to build the UI
ui: frontend/dist/raw/ui

cargo-deps/aarch64-unknown-linux-gnu/release/pi-beep:
	ARCH=aarch64 ./build-cargo-dep.sh pi-beep

cargo-deps/$(ARCH)-unknown-linux-gnu/release/tokio-console: | sudo
	ARCH=$(ARCH) ./build-cargo-dep.sh tokio-console
