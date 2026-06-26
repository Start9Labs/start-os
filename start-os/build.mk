.PHONY: startos
# Build all StartOS OS-product artifacts (bins + web + container-runtime image).
startos: $(STARTOS_TARGETS)

test-container-runtime: start-os/container-runtime/node_modules/.package-lock.json $(call ls-files, start-os/container-runtime/src) start-os/container-runtime/package.json start-os/container-runtime/tsconfig.json 
	cd start-os/container-runtime && npm test

build/lib/migration-images/.done: build/save-migration-images.sh
	ARCH=$(ARCH) ./build/save-migration-images.sh build/lib/migration-images
	touch $@

startos-deb: results/$(BASENAME).deb

results/$(BASENAME).deb: debian/dpkg-build.sh $(call ls-files,debian/startos) $(STARTOS_TARGETS)
	PLATFORM=$(PLATFORM) REQUIRES=debian ./build/os-compat/run-compat.sh ./debian/dpkg-build.sh

startos-$(IMAGE_TYPE): results/$(BASENAME).$(IMAGE_TYPE)

startos-squashfs: results/$(BASENAME).squashfs

results/$(BASENAME).$(IMAGE_TYPE) results/$(BASENAME).squashfs: $(IMAGE_RECIPE_SRC) results/$(BASENAME).deb
	ARCH=$(ARCH) ./build/image-recipe/run-local-build.sh "results/$(BASENAME).deb"

# For creating os images. DO NOT USE
install-startos: $(STARTOS_TARGETS)
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

startos-update-overlay: $(STARTOS_TARGETS)
	@echo "\033[33m!!! THIS WILL ONLY REFLASH YOUR DEVICE IN MEMORY !!!\033[0m"
	@echo "\033[33mALL CHANGES WILL BE REVERTED IF YOU RESTART THE DEVICE\033[0m"
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	@if [ "`ssh $(REMOTE) 'cat /usr/lib/startos/VERSION.txt'`" != "`cat $(VERSION_FILE)`" ]; then >&2 echo "StartOS requires migrations: update-overlay is unavailable." && false; fi
	$(call ssh,"sudo systemctl stop startd")
	$(MAKE) install-startos REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) PLATFORM=$(PLATFORM)
	$(call ssh,"sudo systemctl start startd")

startos-wormhole: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade \"cd /usr/bin && rm startbox && wormhole receive --accept-file %s && chmod +x startbox\"\n", $$3 }'

startos-wormhole-deb: results/$(BASENAME).deb
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send results/$(BASENAME).deb 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo /usr/lib/startos/scripts/chroot-and-upgrade '"'"'cd $$(mktemp -d) && wormhole receive --accept-file %s && apt-get install -y --reinstall ./$(BASENAME).deb'"'"'\n", $$3 }'

startos-wormhole-squashfs: results/$(BASENAME).squashfs
	$(eval SQFS_SUM := $(shell b3sum results/$(BASENAME).squashfs | head -c 32))
	$(eval SQFS_SIZE := $(shell du -s --bytes results/$(BASENAME).squashfs | awk '{print $$1}'))
	@echo "Paste the following command into the shell of your StartOS server:"
	@echo
	@wormhole send results/$(BASENAME).squashfs 2>&1 | awk -Winteractive '/wormhole receive/ { printf "sudo sh -c '"'"'/usr/lib/startos/scripts/prune-images $(SQFS_SIZE) && /usr/lib/startos/scripts/prune-boot && cd /media/startos/images && wormhole receive --accept-file %s && CHECKSUM=$(SQFS_SUM) /usr/lib/startos/scripts/upgrade ./$(BASENAME).squashfs'"'"'\n", $$3 }'

startos-update: $(STARTOS_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(MAKE) install-startos REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/startos/next PLATFORM=$(PLATFORM)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y $(shell cat ./build/lib/depends)"')

startos-update-startbox: target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox # only update binary (faster than full update)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call cp,target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startbox,/media/startos/next/usr/bin/startbox)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync true')

startos-update-deb: results/$(BASENAME).deb # better than update, but only available from debian
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(call mkdir,/media/startos/next/var/tmp/startos-deb)
	$(call cp,results/$(BASENAME).deb,/media/startos/next/var/tmp/startos-deb/$(BASENAME).deb)
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y --reinstall /var/tmp/startos-deb/$(BASENAME).deb"')

startos-update-squashfs: results/$(BASENAME).squashfs
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(eval SQFS_SUM := $(shell b3sum results/$(BASENAME).squashfs | head -c 32))
	$(eval SQFS_SIZE := $(shell du -s --bytes results/$(BASENAME).squashfs | awk '{print $$1}'))
	$(call ssh,'sudo /usr/lib/startos/scripts/prune-images $(SQFS_SIZE)')
	$(call ssh,'sudo /usr/lib/startos/scripts/prune-boot')
	$(call cp,results/$(BASENAME).squashfs,/media/startos/images/next.rootfs)
	$(call ssh,'sudo CHECKSUM=$(SQFS_SUM) /usr/lib/startos/scripts/upgrade /media/startos/images/next.rootfs')

startos-emulate-reflash: $(STARTOS_TARGETS)
	@if [ -z "$(REMOTE)" ]; then >&2 echo "Must specify REMOTE" && false; fi
	$(call ssh,'sudo /usr/lib/startos/scripts/chroot-and-upgrade --create')
	$(MAKE) install-startos REMOTE=$(REMOTE) SSHPASS=$(SSHPASS) DESTDIR=/media/startos/next PLATFORM=$(PLATFORM)
	$(call ssh,'sudo rm -f /media/startos/config/disk.guid /media/startos/config/overlay/etc/hostname')
	$(call ssh,'sudo /media/startos/next/usr/lib/startos/scripts/chroot-and-upgrade --no-sync "apt-get install -y $(shell cat ./build/lib/depends)"')

startos-upload-ota: results/$(BASENAME).squashfs
	TARGET=$(TARGET) KEY=$(KEY) ./build/upload-ota.sh

start-os/container-runtime/debian.$(ARCH).squashfs: ./start-os/container-runtime/download-base-image.sh
	ARCH=$(ARCH) ./start-os/container-runtime/download-base-image.sh

start-os/container-runtime/package-lock.json: start-sdk/dist/package.json
	npm --prefix start-os/container-runtime i
	touch start-os/container-runtime/package-lock.json

start-os/container-runtime/node_modules/.package-lock.json: start-os/container-runtime/package-lock.json
	npm --prefix start-os/container-runtime ci
	touch start-os/container-runtime/node_modules/.package-lock.json

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

# used by github actions
compiled-$(ARCH).tar: $(COMPILED_TARGETS) $(ENVIRONMENT_FILE) $(GIT_HASH_FILE) $(VERSION_FILE)
	tar -cvf $@ $^

target/$(RUST_ARCH)-unknown-linux-musl/release/startos-backup-fs: $(call ls-files, start-os/backup-fs) $(ENVIRONMENT_FILE)
	ARCH=$(ARCH) PROFILE=release ./shared/crates/start-core/build/build-backup-fs.sh
	touch $@