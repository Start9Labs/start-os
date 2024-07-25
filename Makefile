PLATFORM_FILE := $(shell ./check-platform.sh)
ENVIRONMENT_FILE := $(shell ./check-environment.sh)
GIT_HASH_FILE := $(shell ./check-git-hash.sh)
VERSION_FILE := $(shell ./check-version.sh)
BASENAME := $(shell ./basename.sh)
PLATFORM := $(shell if [ -f ./PLATFORM.txt ]; then cat ./PLATFORM.txt; else echo unknown; fi)
ARCH := $(shell if [ "$(PLATFORM)" = "raspberrypi" ]; then echo aarch64; else echo $(PLATFORM) | sed 's/-nonfree$$//g'; fi)
IMAGE_TYPE=$(shell if [ "$(PLATFORM)" = raspberrypi ]; then echo img; else echo iso; fi)
WEB_UIS := web/dist/raw/ui web/dist/raw/setup-wizard web/dist/raw/install-wizard
FIRMWARE_ROMS := ./firmware/$(PLATFORM) $(shell jq --raw-output '.[] | select(.platform[] | contains("$(PLATFORM)")) | "./firmware/$(PLATFORM)/" + .id + ".rom.gz"' build/lib/firmware.json)
BUILD_SRC := $(shell git ls-files build) build/lib/depends build/lib/conflicts $(FIRMWARE_ROMS)
DEBIAN_SRC := $(shell git ls-files debian/)
IMAGE_RECIPE_SRC := $(shell git ls-files image-recipe/)
STARTD_SRC := core/startos/startd.service $(BUILD_SRC)
COMPAT_SRC := $(shell git ls-files system-images/compat/)
UTILS_SRC := $(shell git ls-files system-images/utils/)
BINFMT_SRC := $(shell git ls-files system-images/binfmt/)
CORE_SRC := $(shell git ls-files core) $(shell git ls-files --recurse-submodules patch-db) $(GIT_HASH_FILE)
WEB_SHARED_SRC := $(shell git ls-files web/projects/shared) $(shell ls -p web/ | grep -v / | sed 's/^/web\//g') web/node_modules/.package-lock.json web/config.json patch-db/client/dist web/patchdb-ui-seed.json sdk/dist
WEB_UI_SRC := $(shell git ls-files web/projects/ui)
WEB_SETUP_WIZARD_SRC := $(shell git ls-files web/projects/setup-wizard)
WEB_INSTALL_WIZARD_SRC := $(shell git ls-files web/projects/install-wizard)
PATCH_DB_CLIENT_SRC := $(shell git ls-files --recurse-submodules patch-db/client)
GZIP_BIN := $(shell which pigz || which gzip)
TAR_BIN := $(shell which gtar || which tar)
COMPILED_TARGETS := core/target/$(ARCH)-unknown-linux-musl/release/startbox core/target/$(ARCH)-unknown-linux-musl/release/containerbox system-images/compat/docker-images/$(ARCH).tar system-images/utils/docker-images/$(ARCH).tar system-images/binfmt/docker-images/$(ARCH).tar container-runtime/rootfs.$(ARCH).squashfs
ALL_TARGETS := $(STARTD_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE) $(COMPILED_TARGETS) cargo-deps/$(ARCH)-unknown-linux-musl/release/startos-backup-fs $(shell if [ "$(PLATFORM)" = "raspberrypi" ]; then echo cargo-deps/aarch64-unknown-linux-musl/release/pi-beep; fi)  $(shell /bin/bash -c 'if [[ "${ENVIRONMENT}" =~ (^|-)unstable($$|-) ]]; then echo cargo-deps/$(ARCH)-unknown-linux-musl/release/tokio-console; fi') $(PLATFORM_FILE) 

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

.PHONY: all metadata install clean format cli uis ui reflash deb $(IMAGE_TYPE) squashfs sudo wormhole wormhole-deb test

all: $(ALL_TARGETS)

touch:
	touch $(ALL_TARGETS)

metadata: $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)

sudo:
	sudo true

clean:
	rm -f system-images/**/*.tar
	rm -rf system-images/compat/target
	rm -rf core/target
	rm -rf core/startos/bindings
	rm -rf web/.angular
	rm -f web/config.json
	rm -rf web/node_modules
	rm -rf web/dist
	rm -rf patch-db/client/node_modules
	rm -rf patch-db/client/dist
	rm -rf patch-db/target
	rm -rf cargo-deps
	rm -rf dpkg-workdir
	rm -rf image-recipe/deb
	rm -rf results
	rm -rf build/lib/firmware
	rm -rf container-runtime/dist
	rm -rf container-runtime/node_modules
	rm -f container-runtime/*.squashfs
	rm -rf container-runtime/tmp
	(cd sdk && make clean)
	rm -f ENVIRONMENT.txt
	rm -f PLATFORM.txt
	rm -f GIT_HASH.txt
	rm -f VERSION.txt

format:
	cd core && cargo +nightly fmt

test: $(CORE_SRC) $(ENVIRONMENT_FILE) 
	(cd core && cargo build && cargo test --features=test)
	(cd sdk && make test)

cli:
	cd core && ./install-cli.sh

deb: results/$(BASENAME).deb

debian/control: build/lib/depends build/lib/conflicts
	./debuild/control.sh

results/$(BASENAME).deb: dpkg-build.sh $(DEBIAN_SRC) $(ALL_TARGETS)
	PLATFORM=$(PLATFORM) ./dpkg-build.sh

$(IMAGE_TYPE): results/$(BASENAME).$(IMAGE_TYPE)

squashfs: results/$(BASENAME).squashfs

results/$(BASENAME).$(IMAGE_TYPE) results/$(BASENAME).squashfs: $(IMAGE_RECIPE_SRC) results/$(BASENAME).deb
	./image-recipe/run-local-build.sh "results/$(BASENAME).deb"

# For creating os images. DO NOT USE
install: $(ALL_TARGETS) 
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call mkdir,$(DESTDIR)/usr/sbin)
	$(call cp,core/target/$(ARCH)-unknown-linux-musl/release/startbox,$(DESTDIR)/usr/bin/startbox)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/startd)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-cli)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-sdk)
	if [ "$(PLATFORM)" = "raspberrypi" ]; then $(call cp,cargo-deps/aarch64-unknown-linux-musl/release/pi-beep,$(DESTDIR)/usr/bin/pi-beep); fi
	if /bin/bash -c '[[ "${ENVIRONMENT}" =~ (^|-)unstable($$|-) ]]'; then $(call cp,cargo-deps/$(ARCH)-unknown-linux-musl/release/tokio-console,$(DESTDIR)/usr/bin/tokio-console); fi
	$(call cp,cargo-deps/$(ARCH)-unknown-linux-musl/release/startos-backup-fs,$(DESTDIR)/usr/bin/startos-backup-fs)
	$(call ln,/usr/bin/startos-backup-fs,$(DESTDIR)/usr/sbin/mount.backup-fs)
	
	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,core/startos/startd.service,$(DESTDIR)/lib/systemd/system/startd.service)

	$(call mkdir,$(DESTDIR)/usr/lib)
	$(call rm,$(DESTDIR)/usr/lib/startos)
	$(call cp,build/lib,$(DESTDIR)/usr/lib/startos)
	$(call mkdir,$(DESTDIR)/usr/lib/startos/container-runtime)
	$(call cp,container-runtime/rootfs.$(ARCH).squashfs,$(DESTDIR)/usr/lib/startos/container-runtime/rootfs.squashfs)

	$(call cp,PLATFORM.txt,$(DESTDIR)/usr/lib/startos/PLATFORM.txt)
	$(call cp,ENVIRONMENT.txt,$(DESTDIR)/usr/lib/startos/ENVIRONMENT.txt)
	$(call cp,GIT_HASH.txt,$(DESTDIR)/usr/lib/startos/GIT_HASH.txt)
	$(call cp,VERSION.txt,$(DESTDIR)/usr/lib/startos/VERSION.txt)

	$(call mkdir,$(DESTDIR)/usr/lib/startos/system-images)
	$(call cp,system-images/compat/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/startos/system-images/compat.tar)
	$(call cp,system-images/utils/docker-images/$(ARCH).tar,$(DESTDIR)/usr/lib/startos/system-images/utils.tar)
	
	$(call cp,firmware/$(PLATFORM),$(DESTDIR)/usr/lib/startos/firmware)

update-overlay: $(ALL_TARGETS)
	@echo "\033[33m!!! THIS WILL ONLY REFLASH YOUR DEVICE IN MEMORY !!!\033[0m"
	@echo "\033[33mALL CHANGES WILL BE REVERTED IF YOU RESTART THE DEVICE\033[0m"
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	@if [ "`ssh $(REMOTE) 'cat /usr/lib/startos/VERSION.txt'`" != "`cat ./VERSION.txt`" ]; then >&2 echo "StartOS requires migrations: update-overlay is unavailable." && false; fi
	$(call ssh,"sudo systemctl stop startd")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) PLATFORM=$(PLATFORM)
	$(call ssh,"sudo systemctl start startd")

wormhole: core/target/$(ARCH)-unknown-linux-musl/release/startbox
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send core/target/$(ARCH)-unknown-linux-musl/release/startbox 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade \"cd /usr/bin && rm startbox && wormhole receive --accept-file %s && chmod +x startbox\"\n", $$3 }'

wormhole-deb: results/$(BASENAME).deb
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send results/$(BASENAME).deb 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade '"'"'cd $$(mktemp -d) && wormhole receive --accept-file %s && apt-get install -y --reinstall ./$(BASENAME).deb'"'"'\n", $$3 }'

wormhole-squashfs: results/$(BASENAME).squashfs
	$(eval SQFS_SUM := $(shell b3sum results/$(BASENAME).squashfs | head -c 32))
	$(eval SQFS_SIZE := $(shell du -s --bytes results/$(BASENAME).squashfs | awk '{print $$1}'))
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send results/$(BASENAME).squashfs 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo sh -c '"'"'/usr/lib/startos/scripts/prune-images $(SQFS_SIZE) && cd /media/startos/images && wormhole receive --accept-file %s && mv $(BASENAME).squashfs $(SQFS_SUM).rootfs && ln -rsf ./$(SQFS_SUM).rootfs ../config/current.rootfs && sync && reboot'"'"'\n", $$3 }'

update: $(ALL_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/startos/next PLATFORM=$(PLATFORM)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y $(shell cat ./build/lib/depends)"')

update-startbox: core/target/$(ARCH)-unknown-linux-musl/release/startbox # only update binary (faster than full update)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call cp,core/target/$(ARCH)-unknown-linux-musl/release/startbox,/media/startos/next/usr/bin/startbox)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync true')

update-deb: results/$(BASENAME).deb # better than update, but only available from debian
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call mkdir,/media/startos/next/tmp/startos-deb)
	$(call cp,results/$(BASENAME).deb,/media/startos/next/tmp/startos-deb/$(BASENAME).deb)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y --reinstall /tmp/startos-deb/$(BASENAME).deb"')

update-squashfs: results/$(BASENAME).squashfs
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(eval SQFS_SUM := $(shell b3sum results/$(BASENAME).squashfs))
	$(eval SQFS_SIZE := $(shell du -s --bytes results/$(BASENAME).squashfs | awk '{print $$1}'))
	$(call ssh,'/usr/lib/startos/scripts/prune-images $(SQFS_SIZE)')
	$(call cp,results/$(BASENAME).squashfs,/media/startos/images/$(SQFS_SUM).rootfs)
	$(call ssh,'sudo ln -rsf /media/startos/images/$(SQFS_SUM).rootfs /media/startos/config/current.rootfs')
	$(call ssh,'sudo reboot')

emulate-reflash: $(ALL_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/startos/next PLATFORM=$(PLATFORM)
	$(call ssh,'sudo rm -f /media/startos/config/disk.guid')
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y $(shell cat ./build/lib/depends)"')

upload-ota: results/$(BASENAME).squashfs
	TARGET=$(TARGET) KEY=$(KEY) ./upload-ota.sh

container-runtime/debian.$(ARCH).squashfs:
	ARCH=$(ARCH) ./container-runtime/download-base-image.sh

container-runtime/node_modules: container-runtime/package.json container-runtime/package-lock.json sdk/dist
	npm --prefix container-runtime ci
	touch container-runtime/node_modules

sdk/lib/osBindings: core/startos/bindings
	mkdir -p sdk/lib/osBindings
	ls core/startos/bindings/*.ts | sed 's/core\/startos\/bindings\/\([^.]*\)\.ts/export { \1 } from ".\/\1";/g' > core/startos/bindings/index.ts
	npm --prefix sdk exec -- prettier --config ./sdk/package.json -w ./core/startos/bindings/*.ts
	rsync -ac --delete core/startos/bindings/ sdk/lib/osBindings/
	touch sdk/lib/osBindings

core/startos/bindings: $(shell git ls-files core) $(ENVIRONMENT_FILE)
	rm -rf core/startos/bindings
	(cd core/ && cargo test --features=test '::export_bindings_')
	touch core/startos/bindings

sdk/dist: $(shell git ls-files sdk) sdk/lib/osBindings
	(cd sdk && make bundle)

# TODO: make container-runtime its own makefile?
container-runtime/dist/index.js: container-runtime/node_modules $(shell git ls-files container-runtime/src) container-runtime/package.json container-runtime/tsconfig.json 
	npm --prefix container-runtime run build

container-runtime/dist/node_modules container-runtime/dist/package.json container-runtime/dist/package-lock.json: container-runtime/package.json container-runtime/package-lock.json sdk/dist container-runtime/install-dist-deps.sh
	./container-runtime/install-dist-deps.sh
	touch container-runtime/dist/node_modules

container-runtime/rootfs.$(ARCH).squashfs: container-runtime/debian.$(ARCH).squashfs container-runtime/container-runtime.service container-runtime/update-image.sh container-runtime/deb-install.sh container-runtime/dist/index.js container-runtime/dist/node_modules core/target/$(ARCH)-unknown-linux-musl/release/containerbox | sudo
	ARCH=$(ARCH) ./container-runtime/update-image.sh

build/lib/depends build/lib/conflicts: build/dpkg-deps/*
	build/dpkg-deps/generate.sh

$(FIRMWARE_ROMS): build/lib/firmware.json download-firmware.sh $(PLATFORM_FILE)
	./download-firmware.sh $(PLATFORM)

system-images/compat/docker-images/$(ARCH).tar: $(COMPAT_SRC)
	cd system-images/compat && make docker-images/$(ARCH).tar && touch docker-images/$(ARCH).tar

system-images/utils/docker-images/$(ARCH).tar: $(UTILS_SRC)
	cd system-images/utils && make docker-images/$(ARCH).tar && touch docker-images/$(ARCH).tar

system-images/binfmt/docker-images/$(ARCH).tar: $(BINFMT_SRC)
	cd system-images/binfmt && make docker-images/$(ARCH).tar && touch docker-images/$(ARCH).tar

core/target/$(ARCH)-unknown-linux-musl/release/startbox: $(CORE_SRC) web/dist/static web/patchdb-ui-seed.json $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) ./core/build-startbox.sh
	touch core/target/$(ARCH)-unknown-linux-musl/release/startbox

core/target/$(ARCH)-unknown-linux-musl/release/containerbox: $(CORE_SRC) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) ./core/build-containerbox.sh
	touch core/target/$(ARCH)-unknown-linux-musl/release/containerbox

web/node_modules/.package-lock.json: web/package.json sdk/dist
	npm --prefix web ci
	touch web/node_modules/.package-lock.json

web/.angular: patch-db/client/dist sdk/dist web/node_modules/.package-lock.json
	rm -rf web/.angular
	mkdir -p web/.angular

web/dist/raw/ui: $(WEB_UI_SRC) $(WEB_SHARED_SRC) web/.angular
	npm --prefix web run build:ui
	touch web/dist/raw/ui

web/dist/raw/setup-wizard: $(WEB_SETUP_WIZARD_SRC) $(WEB_SHARED_SRC) web/.angular
	npm --prefix web run build:setup
	touch web/dist/raw/setup-wizard

web/dist/raw/install-wizard: $(WEB_INSTALL_WIZARD_SRC) $(WEB_SHARED_SRC) web/.angular
	npm --prefix web run build:install-wiz
	touch web/dist/raw/install-wizard

web/dist/static: $(WEB_UIS) $(ENVIRONMENT_FILE)
	./compress-uis.sh

web/config.json: $(GIT_HASH_FILE) web/config-sample.json
	jq '.useMocks = false' web/config-sample.json | jq '.gitHash = "$(shell cat GIT_HASH.txt)"' > web/config.json

web/patchdb-ui-seed.json: web/package.json
	jq '."ack-welcome" = $(shell jq '.version' web/package.json)' web/patchdb-ui-seed.json > ui-seed.tmp
	mv ui-seed.tmp web/patchdb-ui-seed.json

patch-db/client/node_modules: patch-db/client/package.json
	npm --prefix patch-db/client ci
	touch patch-db/client/node_modules

patch-db/client/dist: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules
	rm -rf patch-db/client/dist
	npm --prefix patch-db/client run build

# used by github actions
compiled-$(ARCH).tar: $(COMPILED_TARGETS) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE)
	tar -cvf $@ $^

# this is a convenience step to build all web uis - it is not referenced elsewhere in this file
uis: $(WEB_UIS) 

# this is a convenience step to build the UI
ui: web/dist/raw/ui

cargo-deps/aarch64-unknown-linux-musl/release/pi-beep:
	ARCH=aarch64 ./build-cargo-dep.sh pi-beep

cargo-deps/$(ARCH)-unknown-linux-musl/release/tokio-console:
	ARCH=$(ARCH) PREINSTALL="apk add musl-dev pkgconfig" ./build-cargo-dep.sh tokio-console

cargo-deps/$(ARCH)-unknown-linux-musl/release/startos-backup-fs:
	ARCH=$(ARCH) PREINSTALL="apk add fuse3 fuse3-dev fuse3-static musl-dev pkgconfig" ./build-cargo-dep.sh --git https://github.com/Start9Labs/start-fs.git startos-backup-fs