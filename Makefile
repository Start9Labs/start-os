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

.PHONY: all metadata install clean format format-check install-cli cli uis ui reflash deb $(IMAGE_TYPE) squashfs wormhole wormhole-deb test test-core test-sdk test-container-runtime registry install-registry tunnel install-tunnel ts-bindings

all: $(STARTOS_TARGETS)

touch:
	touch $(STARTOS_TARGETS)

metadata: $(VERSION_FILE) $(PLATFORM_FILE) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE)

clean:
	rm -rf target
	rm -rf shared/crates/start-core/bindings
	rm -rf .angular
	rm -f config.json
	rm -rf node_modules
	rm -rf start-os/web/dist
	rm -rf start-tunnel/web/dist
	rm -rf brochure/dist
	rm -rf vendor/patch-db/client/node_modules
	rm -rf vendor/patch-db/client/dist
	rm -rf vendor/patch-db/target
	rm -rf target
	rm -rf dpkg-workdir
	rm -rf image-recipe/deb
	rm -rf results
	rm -rf build/lib/firmware
	rm -rf start-os/container-runtime/dist
	rm -rf start-os/container-runtime/node_modules
	rm -f start-os/container-runtime/*.squashfs
	(cd start-sdk && make clean)
	rm -rf build/lib/migration-images
	rm -f env/*.txt

format:
	cd shared/crates/start-core && cargo +nightly fmt
	npm --prefix . run format
	cd start-sdk && make fmt

# Read-only formatting verification (prettier --check for web + sdk). Used by CI.
format-check:
	npm --prefix . run format:check
	cd start-sdk && make check-fmt

test: | test-core test-sdk test-container-runtime

test-core: $(CORE_SRC) $(ENVIRONMENT_FILE) 
	./shared/crates/start-core/run-tests.sh

test-sdk: $(call ls-files, start-sdk) start-sdk/base/lib/osBindings/index.ts
	cd start-sdk && make test

test-container-runtime: start-os/container-runtime/node_modules/.package-lock.json $(call ls-files, start-os/container-runtime/src) start-os/container-runtime/package.json start-os/container-runtime/tsconfig.json 
	cd start-os/container-runtime && npm test

build/lib/migration-images/.done: build/save-migration-images.sh
	ARCH=$(ARCH) ./build/save-migration-images.sh build/lib/migration-images
	touch $@

install-cli: $(GIT_HASH_FILE)
	./shared/crates/start-core/build/build-cli.sh --install

cli: $(GIT_HASH_FILE)
	./shared/crates/start-core/build/build-cli.sh

registry: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox

install-registry: $(REGISTRY_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox,$(DESTDIR)/usr/bin/start-registrybox)
	$(call ln,/usr/bin/start-registrybox,$(DESTDIR)/usr/bin/start-registryd)
	$(call ln,/usr/bin/start-registrybox,$(DESTDIR)/usr/bin/start-registry)

	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,start-registry/start-registryd.service,$(DESTDIR)/lib/systemd/system/start-registryd.service)

target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/registrybox: $(CORE_SRC) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./shared/crates/start-core/build/build-registrybox.sh

tunnel: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox

install-tunnel: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox start-tunnel/start-tunneld.service
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox,$(DESTDIR)/usr/bin/start-tunnelbox)
	$(call ln,/usr/bin/start-tunnelbox,$(DESTDIR)/usr/bin/start-tunneld)
	$(call ln,/usr/bin/start-tunnelbox,$(DESTDIR)/usr/bin/start-tunnel)

	$(call mkdir,$(DESTDIR)/lib/systemd/system)
	$(call cp,start-tunnel/start-tunneld.service,$(DESTDIR)/lib/systemd/system/start-tunneld.service)

	$(call mkdir,$(DESTDIR)/usr/lib/startos/scripts)
	$(call cp,build/lib/scripts/forward-port,$(DESTDIR)/usr/lib/startos/scripts/forward-port)

	$(call mkdir,$(DESTDIR)/etc/apt/sources.list.d)
	$(call cp,apt/start9.list,$(DESTDIR)/etc/apt/sources.list.d/start9.list)
	$(call mkdir,$(DESTDIR)/usr/share/keyrings)
	$(call cp,apt/start9.gpg,$(DESTDIR)/usr/share/keyrings/start9.gpg)

target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/tunnelbox: $(CORE_SRC) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) start-tunnel/web/dist/static/start-tunnel/index.html
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./shared/crates/start-core/build/build-tunnelbox.sh

deb: results/$(BASENAME).deb

results/$(BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/startos) $(STARTOS_TARGETS)
	PLATFORM=$(PLATFORM) REQUIRES=debian ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh

registry-deb: results/$(REGISTRY_BASENAME).deb

results/$(REGISTRY_BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/start-registry) $(REGISTRY_TARGETS)
	PROJECT=start-registry PLATFORM=$(ARCH) REQUIRES=debian DEPENDS=ca-certificates ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh

tunnel-deb: results/$(TUNNEL_BASENAME).deb

results/$(TUNNEL_BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/start-tunnel) $(TUNNEL_TARGETS) build/lib/scripts/forward-port
	PROJECT=start-tunnel PLATFORM=$(ARCH) REQUIRES=debian DEPENDS=wireguard-tools,iptables,nftables,conntrack ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh

$(IMAGE_TYPE): results/$(BASENAME).$(IMAGE_TYPE)

squashfs: results/$(BASENAME).squashfs

results/$(BASENAME).$(IMAGE_TYPE) results/$(BASENAME).squashfs: $(IMAGE_RECIPE_SRC) results/$(BASENAME).deb
	ARCH=$(ARCH) ./build/image-recipe/run-local-build.sh "results/$(BASENAME).deb"

# For creating os images. DO NOT USE
install: $(STARTOS_TARGETS)
	$(call mkdir,$(DESTDIR)/usr/bin)
	$(call mkdir,$(DESTDIR)/usr/sbin)
	$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox,$(DESTDIR)/usr/bin/startbox)
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
	$(call cp,start-os/startd.service,$(DESTDIR)/lib/systemd/system/startd.service)
	$(call cp,start-os/services.slice,$(DESTDIR)/lib/systemd/system/services.slice)
	$(call cp,start-os/startos-shutdown.service,$(DESTDIR)/lib/systemd/system/startos-shutdown.service)
	$(call cp,start-os/startos-restart.service,$(DESTDIR)/lib/systemd/system/startos-restart.service)
	if /bin/bash -c '[[ "${ENVIRONMENT}" =~ (^|-)unstable($$|-) ]]'; then \
		sed -i '/^Environment=/a Environment=RUST_BACKTRACE=full' $(DESTDIR)/lib/systemd/system/startd.service; \
	fi

	$(call mkdir,$(DESTDIR)/usr/lib)
	$(call rm,$(DESTDIR)/usr/lib/startos)
	$(call cp,build/lib,$(DESTDIR)/usr/lib/startos)
	$(call mkdir,$(DESTDIR)/usr/lib/startos/container-runtime)
	$(call cp,start-os/container-runtime/rootfs.$(ARCH).squashfs,$(DESTDIR)/usr/lib/startos/container-runtime/rootfs.squashfs)

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

wormhole: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade \"cd /usr/bin && rm startbox && wormhole receive --accept-file %s && chmod +x startbox\"\n", $$3 }'

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

update-startbox: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox # only update binary (faster than full update)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox,/media/startos/next/usr/bin/startbox)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync true')

update-deb: results/$(BASENAME).deb # better than update, but only available from debian
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call mkdir,/media/startos/next/var/tmp/startos-deb)
	$(call cp,results/$(BASENAME).deb,/media/startos/next/var/tmp/startos-deb/$(BASENAME).deb)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y --reinstall /var/tmp/startos-deb/$(BASENAME).deb"')

update-squashfs: results/$(BASENAME).squashfs
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(eval SQFS_SUM := $(shell b3sum results/$(BASENAME).squashfs | head -c 32))
	$(eval SQFS_SIZE := $(shell du -s --bytes results/$(BASENAME).squashfs | awk '{print $$1}'))
	$(call ssh,'sudo /usr/lib/startos/scripts/prune-images $(SQFS_SIZE)')
	$(call ssh,'sudo /usr/lib/startos/scripts/prune-boot')
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

start-os/container-runtime/debian.$(ARCH).squashfs: ./start-os/container-runtime/download-base-image.sh
	ARCH=$(ARCH) ./start-os/container-runtime/download-base-image.sh

start-os/container-runtime/package-lock.json: start-sdk/dist/package.json
	npm --prefix start-os/container-runtime i
	touch start-os/container-runtime/package-lock.json

start-os/container-runtime/node_modules/.package-lock.json: start-os/container-runtime/package-lock.json
	npm --prefix start-os/container-runtime ci
	touch start-os/container-runtime/node_modules/.package-lock.json

ts-bindings: shared/crates/start-core/bindings/index.ts
	mkdir -p start-sdk/base/lib/osBindings
	rsync -ac --delete shared/crates/start-core/bindings/ start-sdk/base/lib/osBindings/

shared/crates/start-core/bindings/index.ts: $(call ls-files, shared/crates/start-core) $(ENVIRONMENT_FILE)
	rm -rf shared/crates/start-core/bindings
	./shared/crates/start-core/build/build-ts.sh
	ls shared/crates/start-core/bindings/*.ts | sed 's|.*/bindings/\([^.]*\)\.ts|export { \1 } from "./\1";|g' | grep -v '"./index"' | tee shared/crates/start-core/bindings/index.ts
	if [ -d shared/crates/start-core/bindings/tunnel ]; then \
		ls shared/crates/start-core/bindings/tunnel/*.ts | sed 's|.*/bindings/tunnel/\([^.]*\)\.ts|export { \1 } from "./\1";|g' | grep -v '"./index"' > shared/crates/start-core/bindings/tunnel/index.ts; \
		echo 'export * as Tunnel from "./tunnel";' >> shared/crates/start-core/bindings/index.ts; \
	fi
	npm --prefix start-sdk/base exec -- prettier --config=./start-sdk/base/package.json -w './shared/crates/start-core/bindings/**/*.ts'
	touch shared/crates/start-core/bindings/index.ts

start-sdk/dist/package.json start-sdk/baseDist/package.json: $(call ls-files, start-sdk) start-sdk/base/lib/osBindings/index.ts
	(cd start-sdk && make bundle)
	touch start-sdk/dist/package.json
	touch start-sdk/baseDist/package.json

# TODO: make container-runtime its own makefile?
start-os/container-runtime/dist/index.js: start-os/container-runtime/node_modules/.package-lock.json $(call ls-files, start-os/container-runtime/src) start-os/container-runtime/package.json start-os/container-runtime/tsconfig.json 
	npm --prefix start-os/container-runtime run build

start-os/container-runtime/dist/node_modules/.package-lock.json start-os/container-runtime/dist/package.json start-os/container-runtime/dist/package-lock.json: start-os/container-runtime/package.json start-os/container-runtime/package-lock.json start-sdk/dist/package.json start-os/container-runtime/install-dist-deps.sh
	./start-os/container-runtime/install-dist-deps.sh
	touch start-os/container-runtime/dist/node_modules/.package-lock.json

start-os/container-runtime/rootfs.$(ARCH).squashfs: start-os/container-runtime/debian.$(ARCH).squashfs start-os/container-runtime/container-runtime.service start-os/container-runtime/update-image.sh start-os/container-runtime/update-image-local.sh start-os/container-runtime/deb-install.sh start-os/container-runtime/dist/index.js start-os/container-runtime/dist/node_modules/.package-lock.json target/$(RUST_ARCH)-unknown-linux-musl/release/start-container
	ARCH=$(ARCH) ./start-os/container-runtime/update-image-local.sh

build/lib/depends build/lib/conflicts: $(ENVIRONMENT_FILE) $(PLATFORM_FILE) $(shell ls build/dpkg-deps/*)
	PLATFORM=$(PLATFORM) ARCH=$(ARCH) build/dpkg-deps/generate.sh

$(FIRMWARE_ROMS): build/lib/firmware.json ./build/download-firmware.sh $(PLATFORM_FILE)
	./build/download-firmware.sh $(PLATFORM)

target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox: $(CORE_SRC) $(COMPRESSED_WEB_UIS) start-os/web/patchdb-ui-seed.json $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) PROFILE=$(PROFILE) ./shared/crates/start-core/build/build-startbox.sh
	touch target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox

target/$(RUST_ARCH)-unknown-linux-musl/release/start-container: $(CORE_SRC) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) ./shared/crates/start-core/build/build-start-container.sh
	touch target/$(RUST_ARCH)-unknown-linux-musl/release/start-container

package-lock.json: package.json start-sdk/baseDist/package.json
	npm --prefix . i
	touch package-lock.json

node_modules/.package-lock.json: package-lock.json
	npm --prefix . ci
	touch node_modules/.package-lock.json

.angular/.updated: vendor/patch-db/client/dist/index.js start-sdk/baseDist/package.json node_modules/.package-lock.json
	rm -rf .angular
	mkdir -p .angular
	touch .angular/.updated

.i18n-checked: $(WEB_SHARED_SRC) $(WEB_UI_SRC) $(WEB_SETUP_WIZARD_SRC) $(WEB_START_TUNNEL_SRC)
	npm --prefix . run check:i18n
	touch .i18n-checked

start-os/web/dist/raw/ui/index.html: $(WEB_UI_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:ui
	touch start-os/web/dist/raw/ui/index.html

start-os/web/dist/raw/setup-wizard/index.html: $(WEB_SETUP_WIZARD_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:setup
	touch start-os/web/dist/raw/setup-wizard/index.html

start-tunnel/web/dist/raw/start-tunnel/index.html: $(WEB_START_TUNNEL_SRC) $(WEB_SHARED_SRC) .angular/.updated .i18n-checked
	npm --prefix . run build:tunnel
	touch start-tunnel/web/dist/raw/start-tunnel/index.html

start-os/web/dist/static/%/index.html: start-os/web/dist/raw/%/index.html
	./shared/web/compress-uis.sh $* start-os/web

start-tunnel/web/dist/static/%/index.html: start-tunnel/web/dist/raw/%/index.html
	./shared/web/compress-uis.sh $* start-tunnel/web

config.json: $(GIT_HASH_FILE) $(ENVIRONMENT_FILE) shared/web/config-sample.json shared/web/update-config.sh
	./shared/web/update-config.sh	

vendor/patch-db/client/node_modules/.package-lock.json: vendor/patch-db/client/package.json
	npm --prefix vendor/patch-db/client ci
	touch vendor/patch-db/client/node_modules/.package-lock.json

vendor/patch-db/client/dist/index.js: $(PATCH_DB_CLIENT_SRC) vendor/patch-db/client/node_modules/.package-lock.json
	rm -rf vendor/patch-db/client/dist
	npm --prefix vendor/patch-db/client run build
	touch vendor/patch-db/client/dist/index.js

# used by github actions
compiled-$(ARCH).tar: $(COMPILED_TARGETS) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE)
	tar -cvf $@ $^

# this is a convenience step to build all web uis - it is not referenced elsewhere in this file
uis: $(WEB_UIS) 

# this is a convenience step to build the UI
ui: start-os/web/dist/raw/ui

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
