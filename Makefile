ls-files = $(shell git ls-files --cached --others --exclude-standard $1) 
PROFILE = release

PLATFORM_FILE := $(shell ./build/env/check-platform.sh)
ENVIRONMENT_FILE := $(shell ./build/env/check-environment.sh)
GIT_HASH_FILE := $(shell ./build/env/check-git-hash.sh)
VERSION_FILE := $(shell ./build/env/check-version.sh)
BASENAME := $(shell PROJECT=startos ./build/env/basename.sh)
PLATFORM := $(shell if [ -f $(PLATFORM_FILE) ]; then cat $(PLATFORM_FILE); else echo unknown; fi)
ARCH := $(shell if [ "$(PLATFORM)" = "raspberrypi" ]; then echo aarch64; else echo $(PLATFORM) | sed 's/-nonfree$$//g'; fi)
RUST_ARCH := $(shell if [ "$(ARCH)" = "riscv64" ]; then echo riscv64gc; else echo $(ARCH); fi)
REGISTRY_BASENAME := $(shell PROJECT=start-registry PLATFORM=$(ARCH) ./build/env/basename.sh)
TUNNEL_BASENAME := $(shell PROJECT=start-tunnel PLATFORM=$(ARCH) ./build/env/basename.sh)
IMAGE_TYPE=$(shell if [ "$(PLATFORM)" = raspberrypi ]; then echo img; else echo iso; fi)
WEB_UIS := web/dist/raw/ui/index.html web/dist/raw/setup-wizard/index.html
COMPRESSED_WEB_UIS := web/dist/static/ui/index.html web/dist/static/setup-wizard/index.html
FIRMWARE_ROMS := build/lib/firmware/$(PLATFORM) $(shell jq --raw-output '.[] | select(.platform[] | contains("$(PLATFORM)")) | "./build/lib/firmware/$(PLATFORM)/" + .id + ".rom.gz"' build/lib/firmware.json)
BUILD_SRC := $(call ls-files, build/lib) build/lib/depends build/lib/conflicts $(FIRMWARE_ROMS)
IMAGE_RECIPE_SRC := $(call ls-files, build/image-recipe/)
STARTD_SRC := core/startd.service $(BUILD_SRC)
CORE_SRC := $(call ls-files, core) $(shell git ls-files --recurse-submodules patch-db) $(GIT_HASH_FILE)
WEB_SHARED_SRC := $(call ls-files, web/projects/shared) $(call ls-files, web/projects/marketplace) $(shell ls -p web/ | grep -v / | sed 's/^/web\//g') web/node_modules/.package-lock.json web/config.json patch-db/client/dist/index.js sdk/baseDist/package.json web/patchdb-ui-seed.json sdk/dist/package.json
WEB_UI_SRC := $(call ls-files, web/projects/ui)
WEB_SETUP_WIZARD_SRC := $(call ls-files, web/projects/setup-wizard)
WEB_START_TUNNEL_SRC := $(call ls-files, web/projects/start-tunnel)
PATCH_DB_CLIENT_SRC := $(shell git ls-files --recurse-submodules patch-db/client)
GZIP_BIN := $(shell which pigz || which gzip)
TAR_BIN := $(shell which gtar || which tar)
COMPILED_TARGETS := core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox core/target/$(RUST_ARCH)-unknown-linux-musl/release/start-container container-runtime/rootfs.$(ARCH).squashfs
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
REGISTRY_TARGETS := core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox core/start-registryd.service
TUNNEL_TARGETS := core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox core/start-tunneld.service

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

.PHONY: all metadata install clean format install-cli cli uis ui reflash deb $(IMAGE_TYPE) squashfs wormhole wormhole-deb test test-core test-sdk test-container-runtime registry install-registry tunnel install-tunnel ts-bindings

all: $(STARTOS_TARGETS)

touch:
	touch $(STARTOS_TARGETS)

metadata: $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)

clean:
	rm -rf core/target
	rm -rf core/bindings
	rm -rf web/.angular
	rm -f web/config.json
	rm -rf web/node_modules
	rm -rf web/dist
	rm -rf patch-db/client/node_modules
	rm -rf patch-db/client/dist
	rm -rf patch-db/target
	rm -rf target
	rm -rf dpkg-workdir
	rm -rf image-recipe/deb
	rm -rf results
	rm -rf build/lib/firmware
	rm -rf container-runtime/dist
	rm -rf container-runtime/node_modules
	rm -f container-runtime/*.squashfs
	(cd sdk && make clean)
	rm -f env/*.txt

format:
	cd core && cargo +nightly fmt

test: | test-core test-sdk test-container-runtime

test-core: $(CORE_SRC) $(ENVIRONMENT_FILE) 
	./core/run-tests.sh

test-sdk: $(call ls-files, sdk) sdk/base/lib/osBindings/index.ts
	cd sdk && make test

test-container-runtime: container-runtime/node_modules/.package-lock.json $(call ls-files, container-runtime/src) container-runtime/package.json container-runtime/tsconfig.json 
	cd container-runtime && npm test

install-cli: $(GIT_HASH_FILE)
	./core/build/build-cli.sh --install

cli: $(GIT_HASH_FILE)
	./core/build/build-cli.sh

registry: core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox

install-registry: $(REGISTRY_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox,$(DESTDIR)/usr/bin/start-registrybox)
	$(call ln,/usr/bin/start-registrybox,$(DESTDIR)/usr/bin/start-registryd)
	$(call ln,/usr/bin/start-registrybox,$(DESTDIR)/usr/bin/start-registry)

	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,core/start-registryd.service,$(DESTDIR)/lib/systemd/system/start-registryd.service)

core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox: $(CORE_SRC) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./core/build/build-registrybox.sh

tunnel: core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox

install-tunnel: core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox core/start-tunneld.service
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox,$(DESTDIR)/usr/bin/start-tunnelbox)
	$(call ln,/usr/bin/start-tunnelbox,$(DESTDIR)/usr/bin/start-tunneld)
	$(call ln,/usr/bin/start-tunnelbox,$(DESTDIR)/usr/bin/start-tunnel)

	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,core/start-tunneld.service,$(DESTDIR)/lib/systemd/system/start-tunneld.service)

	$(call mkdir,$(DESTDIR)/usr/lib/startos/scripts)
	$(call cp,build/lib/scripts/forward-port,$(DESTDIR)/usr/lib/startos/scripts/forward-port)

	$(call mkdir,$(DESTDIR)/etc/apt/sources.list.d)
	$(call cp,apt/start9.list,$(DESTDIR)/etc/apt/sources.list.d/start9.list)
	$(call mkdir,$(DESTDIR)/usr/share/keyrings)
	$(call cp,apt/start9.gpg,$(DESTDIR)/usr/share/keyrings/start9.gpg)

core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox: $(CORE_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) web/dist/static/start-tunnel/index.html
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./core/build/build-tunnelbox.sh

deb: results/$(BASENAME).deb

results/$(BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/startos) $(STARTOS_TARGETS)
	PLATFORM=$(PLATFORM) REQUIRES=debian ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh

registry-deb: results/$(REGISTRY_BASENAME).deb

results/$(REGISTRY_BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/start-registry) $(REGISTRY_TARGETS)
	PROJECT=start-registry PLATFORM=$(ARCH) REQUIRES=debian ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh

tunnel-deb: results/$(TUNNEL_BASENAME).deb

results/$(TUNNEL_BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/start-tunnel) $(TUNNEL_TARGETS) build/lib/scripts/forward-port
	PROJECT=start-tunnel PLATFORM=$(ARCH) REQUIRES=debian DEPENDS=wireguard-tools,iptables,conntrack ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh

$(IMAGE_TYPE): results/$(BASENAME).$(IMAGE_TYPE)

squashfs: results/$(BASENAME).squashfs

results/$(BASENAME).$(IMAGE_TYPE) results/$(BASENAME).squashfs: $(IMAGE_RECIPE_SRC) results/$(BASENAME).deb
	ARCH=$(ARCH) ./build/image-recipe/run-local-build.sh "results/$(BASENAME).deb"

# For creating os images. DO NOT USE
install: $(STARTOS_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call mkdir,$(DESTDIR)/usr/sbin)
	$(call cp,core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox,$(DESTDIR)/usr/bin/startbox)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/startd)
	$(call ln,/usr/bin/startbox,$(DESTDIR)/usr/bin/start-cli)
	if [ "$(PLATFORM)" = "raspberrypi" ]; then $(call cp,target/aarch64-unknown-linux-musl/release/pi-beep,$(DESTDIR)/usr/bin/pi-beep); fi
	if /bin/bash -c '[[ "${ENVIRONMENT}" =~ (^|-)unstable($$|-) ]]'; then \
		$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/release/flamegraph,$(DESTDIR)/usr/bin/flamegraph); \
	fi
	if /bin/bash -c '[[ "${ENVIRONMENT}" =~ (^|-)console($$|-) ]]'; then \
		$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/release/tokio-console,$(DESTDIR)/usr/bin/tokio-console); \
	fi
	$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/release/startos-backup-fs,$(DESTDIR)/usr/bin/startos-backup-fs)
	$(call ln,/usr/bin/startos-backup-fs,$(DESTDIR)/usr/sbin/mount.backup-fs)
	
	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,core/startd.service,$(DESTDIR)/lib/systemd/system/startd.service)

	$(call mkdir,$(DESTDIR)/usr/lib)
	$(call rm,$(DESTDIR)/usr/lib/startos)
	$(call cp,build/lib,$(DESTDIR)/usr/lib/startos)
	$(call mkdir,$(DESTDIR)/usr/lib/startos/container-runtime)
	$(call cp,container-runtime/rootfs.$(ARCH).squashfs,$(DESTDIR)/usr/lib/startos/container-runtime/rootfs.squashfs)

	$(call cp,build/env/PLATFORM.txt,$(DESTDIR)/usr/lib/startos/PLATFORM.txt)
	$(call cp,build/env/ENVIRONMENT.txt,$(DESTDIR)/usr/lib/startos/ENVIRONMENT.txt)
	$(call cp,build/env/GIT_HASH.txt,$(DESTDIR)/usr/lib/startos/GIT_HASH.txt)
	$(call cp,build/env/VERSION.txt,$(DESTDIR)/usr/lib/startos/VERSION.txt)

update-overlay: $(STARTOS_TARGETS)
	@echo "\033[33m!!! THIS WILL ONLY REFLASH YOUR DEVICE IN MEMORY !!!\033[0m"
	@echo "\033[33mALL CHANGES WILL BE REVERTED IF YOU RESTART THE DEVICE\033[0m"
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	@if [ "`ssh $(REMOTE) 'cat /usr/lib/startos/VERSION.txt'`" != "`cat $(VERSION_FILE)`" ]; then >&2 echo "StartOS requires migrations: update-overlay is unavailable." && false; fi
	$(call ssh,"sudo systemctl stop startd")
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) PLATFORM=$(PLATFORM)
	$(call ssh,"sudo systemctl start startd")

wormhole: core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade \"cd /usr/bin && rm startbox && wormhole receive --accept-file %s && chmod +x startbox\"\n", $$3 }'

wormhole-deb: results/$(BASENAME).deb
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send results/$(BASENAME).deb 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade '"'"'cd $$(mktemp -d) && wormhole receive --accept-file %s && apt-get install -y --reinstall ./$(BASENAME).deb'"'"'\n", $$3 }'

wormhole-squashfs: results/$(BASENAME).squashfs
	$(eval SQFS_SUM := $(shell b3sum results/$(BASENAME).squashfs | head -c 32))
	$(eval SQFS_SIZE := $(shell du -s --bytes results/$(BASENAME).squashfs | awk '{print $$1}'))
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send results/$(BASENAME).squashfs 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo sh -c '"'"'/usr/lib/startos/scripts/prune-images $(SQFS_SIZE) && /usr/lib/startos/scripts/prune-boot && cd /media/startos/images && wormhole receive --accept-file %s && CHECKSUM=$(SQFS_SUM) /usr/lib/startos/scripts/upgrade ./$(BASENAME).squashfs'"'"'\n", $$3 }'

update: $(STARTOS_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/startos/next PLATFORM=$(PLATFORM)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y $(shell cat ./build/lib/depends)"')

update-startbox: core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox # only update binary (faster than full update)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call cp,core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox,/media/startos/next/usr/bin/startbox)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync true')

update-deb: results/$(BASENAME).deb # better than update, but only available from debian
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call mkdir,/media/startos/next/var/tmp/startos-deb)
	$(call cp,results/$(BASENAME).deb,/media/startos/next/var/tmp/startos-deb/$(BASENAME).deb)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y --reinstall /var/tmp/startos-deb/$(BASENAME).deb"')

update-squashfs: results/$(BASENAME).squashfs
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(eval SQFS_SUM := $(shell b3sum results/$(BASENAME).squashfs))
	$(eval SQFS_SIZE := $(shell du -s --bytes results/$(BASENAME).squashfs | awk '{print $$1}'))
	$(call ssh,'/usr/lib/startos/scripts/prune-images $(SQFS_SIZE)')
	$(call ssh,'/usr/lib/startos/scripts/prune-boot')
	$(call cp,results/$(BASENAME).squashfs,/media/startos/images/next.rootfs)
	$(call ssh,'sudo CHECKSUM=$(SQFS_SUM) /usr/lib/startos/scripts/upgrade /media/startos/images/next.rootfs')

emulate-reflash: $(STARTOS_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(MAKE) install REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/startos/next PLATFORM=$(PLATFORM)
	$(call ssh,'sudo rm -f /media/startos/config/disk.guid /media/startos/config/overlay/etc/hostname')
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y $(shell cat ./build/lib/depends)"')

upload-ota: results/$(BASENAME).squashfs
	TARGET=$(TARGET) KEY=$(KEY) ./build/upload-ota.sh

container-runtime/debian.$(ARCH).squashfs: ./container-runtime/download-base-image.sh
	ARCH=$(ARCH) ./container-runtime/download-base-image.sh

container-runtime/package-lock.json: sdk/dist/package.json
	npm --prefix container-runtime i
	touch container-runtime/package-lock.json

container-runtime/node_modules/.package-lock.json: container-runtime/package-lock.json
	npm --prefix container-runtime ci
	touch container-runtime/node_modules/.package-lock.json

ts-bindings: core/bindings/index.ts
	mkdir -p sdk/base/lib/osBindings
	rsync -ac --delete core/bindings/ sdk/base/lib/osBindings/

core/bindings/index.ts: $(call ls-files, core) $(ENVIRONMENT_FILE)
	rm -rf core/bindings
	./core/build/build-ts.sh
	ls core/bindings/*.ts | sed 's/core\/bindings\/\([^.]*\)\.ts/export { \1 } from ".\/\1";/g' | grep -v '"./index"' | tee core/bindings/index.ts
	npm --prefix sdk/base exec -- prettier --config=./sdk/base/package.json -w ./core/bindings/*.ts
	touch core/bindings/index.ts

sdk/dist/package.json sdk/baseDist/package.json: $(call ls-files, sdk) sdk/base/lib/osBindings/index.ts
	(cd sdk && make bundle)
	touch sdk/dist/package.json
	touch sdk/baseDist/package.json

# TODO: make container-runtime its own makefile?
container-runtime/dist/index.js: container-runtime/node_modules/.package-lock.json $(call ls-files, container-runtime/src) container-runtime/package.json container-runtime/tsconfig.json 
	npm --prefix container-runtime run build

container-runtime/dist/node_modules/.package-lock.json container-runtime/dist/package.json container-runtime/dist/package-lock.json: container-runtime/package.json container-runtime/package-lock.json sdk/dist/package.json container-runtime/install-dist-deps.sh
	./container-runtime/install-dist-deps.sh
	touch container-runtime/dist/node_modules/.package-lock.json

container-runtime/rootfs.$(ARCH).squashfs: container-runtime/debian.$(ARCH).squashfs container-runtime/container-runtime.service container-runtime/update-image.sh container-runtime/update-image-local.sh container-runtime/deb-install.sh container-runtime/dist/index.js container-runtime/dist/node_modules/.package-lock.json core/target/$(RUST_ARCH)-unknown-linux-musl/release/start-container
	ARCH=$(ARCH) ./container-runtime/update-image-local.sh

build/lib/depends build/lib/conflicts: $(ENVIRONMENT_FILE) $(PLATFORM_FILE) $(shell ls build/dpkg-deps/*)
	PLATFORM=$(PLATFORM) ARCH=$(ARCH) build/dpkg-deps/generate.sh

$(FIRMWARE_ROMS): build/lib/firmware.json ./build/download-firmware.sh $(PLATFORM_FILE)
	./build/download-firmware.sh $(PLATFORM)

core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox: $(CORE_SRC) $(COMPRESSED_WEB_UIS) web/patchdb-ui-seed.json $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./core/build/build-startbox.sh
	touch core/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox

core/target/$(RUST_ARCH)-unknown-linux-musl/release/start-container: $(CORE_SRC) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) ./core/build/build-start-container.sh
	touch core/target/$(RUST_ARCH)-unknown-linux-musl/release/start-container

web/package-lock.json: web/package.json sdk/baseDist/package.json
	npm --prefix web i
	touch web/package-lock.json

web/node_modules/.package-lock.json: web/package-lock.json
	npm --prefix web ci
	touch web/node_modules/.package-lock.json

web/.angular/.updated: patch-db/client/dist/index.js sdk/baseDist/package.json web/node_modules/.package-lock.json
	rm -rf web/.angular
	mkdir -p web/.angular
	touch web/.angular/.updated

web/.i18n-checked: $(WEB_SHARED_SRC) $(WEB_UI_SRC) $(WEB_SETUP_WIZARD_SRC) $(WEB_START_TUNNEL_SRC)
	npm --prefix web run check:i18n
	touch web/.i18n-checked

web/dist/raw/ui/index.html: $(WEB_UI_SRC) $(WEB_SHARED_SRC) web/.angular/.updated web/.i18n-checked
	npm --prefix web run build:ui
	touch web/dist/raw/ui/index.html

web/dist/raw/setup-wizard/index.html: $(WEB_SETUP_WIZARD_SRC) $(WEB_SHARED_SRC) web/.angular/.updated web/.i18n-checked
	npm --prefix web run build:setup
	touch web/dist/raw/setup-wizard/index.html

web/dist/raw/start-tunnel/index.html: $(WEB_START_TUNNEL_SRC) $(WEB_SHARED_SRC) web/.angular/.updated web/.i18n-checked
	npm --prefix web run build:tunnel
	touch web/dist/raw/start-tunnel/index.html

web/dist/static/%/index.html: web/dist/raw/%/index.html
	./web/compress-uis.sh $*

web/config.json: $(GIT_HASH_FILE) $(ENVIRONMENT_FILE) web/config-sample.json web/update-config.sh
	./web/update-config.sh	

patch-db/client/node_modules/.package-lock.json: patch-db/client/package.json
	npm --prefix patch-db/client ci
	touch patch-db/client/node_modules/.package-lock.json

patch-db/client/dist/index.js: $(PATCH_DB_CLIENT_SRC) patch-db/client/node_modules/.package-lock.json
	rm -rf patch-db/client/dist
	npm --prefix patch-db/client run build
	touch patch-db/client/dist/index.js

# used by github actions
compiled-$(ARCH).tar: $(COMPILED_TARGETS) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE)
	tar -cvf $@ $^

# this is a convenience step to build all web uis - it is not referenced elsewhere in this file
uis: $(WEB_UIS) 

# this is a convenience step to build the UI
ui: web/dist/raw/ui

target/aarch64-unknown-linux-musl/release/pi-beep: ./build/build-cargo-dep.sh
	ARCH=aarch64 ./build/build-cargo-dep.sh pi-beep

target/$(RUST_ARCH)-unknown-linux-musl/release/tokio-console: ./build/build-cargo-dep.sh
	ARCH=$(ARCH) ./build/build-cargo-dep.sh tokio-console
	touch $@

target/$(RUST_ARCH)-unknown-linux-musl/release/startos-backup-fs: ./build/build-cargo-dep.sh
	ARCH=$(ARCH) ./build/build-cargo-dep.sh --git https://github.com/Start9Labs/start-fs.git startos-backup-fs
	touch $@

target/$(RUST_ARCH)-unknown-linux-musl/release/flamegraph: ./build/build-cargo-dep.sh
	ARCH=$(ARCH) ./build/build-cargo-dep.sh flamegraph
	touch $@
