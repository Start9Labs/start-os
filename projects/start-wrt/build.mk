# When this product's build inputs change, mirror them into the `paths:` filter
# of .github/workflows/start-wrt.yaml (see root AGENTS.md "Coupled changes").
#
# UNVALIDATED: the riscv dockerized zigbuild and the OpenWrt image assembly
# (stage / image targets) have NOT been run since the monorepo migration. The
# binary+web build (the `startwrt` target) is the first thing to validate on a
# build host; the image targets follow. See projects/start-wrt/CONTRIBUTING.md.

STARTWRT_DIR := projects/start-wrt
STARTWRT_RUST_ARCH := riscv64gc
STARTWRT_ARCH := riscv64

STARTWRT_BIN := target/$(STARTWRT_RUST_ARCH)-unknown-linux-musl/$(PROFILE)/startwrt
STARTWRT_WEB_DIST := $(STARTWRT_DIR)/web/dist/startwrt/browser/index.html
STARTWRT_WEB_CONFIG := $(STARTWRT_DIR)/web/config.json

STARTWRT_RUST_SRC := $(call ls-files, $(STARTWRT_DIR)/backend)
STARTWRT_WEB_SRC := $(call ls-files, $(STARTWRT_DIR)/web/src)

STARTWRT_OPENWRT := $(STARTWRT_DIR)/openwrt
STARTWRT_IMAGE_DIR := $(STARTWRT_OPENWRT)/bin/targets/spacemit
STARTWRT_SDCARD_NAME := openwrt-spacemit-k1-sbc-bananapi-f3-squashfs-sdcard.img
STARTWRT_SYSUPGRADE_NAME := openwrt-spacemit-k1-sbc-bananapi-f3-squashfs-sysupgrade.img.gz
STARTWRT_IMAGES := results/$(STARTWRT_SDCARD_NAME) results/$(STARTWRT_SYSUPGRADE_NAME)

# Remote deploy target (binary only — web UI is embedded in the binary).
STARTWRT_REMOTE ?= root@192.168.0.1

.PHONY: startwrt
# Build the startwrt binary (embeds the web UI). Cross-compiled for the K1 via
# the dockerized cargo-zigbuild toolchain in build/build-rust.sh.
startwrt: $(STARTWRT_BIN)

$(STARTWRT_BIN): $(STARTWRT_RUST_SRC) $(STARTWRT_WEB_DIST) $(STARTWRT_DIR)/build/build-rust.sh
	ARCH=$(STARTWRT_ARCH) RUST_ARCH=$(STARTWRT_RUST_ARCH) PROFILE=$(PROFILE) ./$(STARTWRT_DIR)/build/build-rust.sh
	@touch $(STARTWRT_BIN)

# --- web (standalone Angular workspace in Stage A; own package.json) ---
$(STARTWRT_WEB_DIST): $(STARTWRT_WEB_SRC) $(STARTWRT_DIR)/web/package-lock.json $(STARTWRT_WEB_CONFIG)
	npm --prefix $(STARTWRT_DIR)/web ci
	npm --prefix $(STARTWRT_DIR)/web run build

# Prod config: useMocks=false + gitHash stamped from the product's own
# build/env/GIT_HASH.txt (independent of the monorepo-root stamp).
$(STARTWRT_WEB_CONFIG): $(STARTWRT_DIR)/web/config-sample.json $(STARTWRT_DIR)/web/update-config.sh
	./$(STARTWRT_DIR)/build/env/check-git-hash.sh >/dev/null
	./$(STARTWRT_DIR)/web/update-config.sh

# Run start-wrt's Rust unit tests (startwrt-core + uciedit), package-scoped and
# containerized — mirrors test-core. See build/run-tests.sh.
.PHONY: test-startwrt
test-startwrt: $(STARTWRT_RUST_SRC) $(ENVIRONMENT_FILE)
	./$(STARTWRT_DIR)/build/run-tests.sh

# --- OpenWrt image (HEAVY, UNVALIDATED) ---
# One-time feeds/config/download.
.PHONY: startwrt-openwrt-setup
startwrt-openwrt-setup: $(STARTWRT_OPENWRT)/.config
$(STARTWRT_OPENWRT)/.config: $(STARTWRT_DIR)/build/openwrt.diffconfig $(STARTWRT_DIR)/build/feeds.conf
	./$(STARTWRT_DIR)/build/openwrt-setup.sh

# Stage the binary + UCI configs + init scripts into openwrt/files/.
.PHONY: startwrt-stage
startwrt-stage: $(STARTWRT_OPENWRT)/files/.staged
$(STARTWRT_OPENWRT)/files/.staged: $(STARTWRT_BIN) $(call ls-files, $(STARTWRT_DIR)/backend/firstboot_config) $(STARTWRT_DIR)/build/stage-files.sh
	ARCH=$(STARTWRT_ARCH) RUST_ARCH=$(STARTWRT_RUST_ARCH) PROFILE=$(PROFILE) ./$(STARTWRT_DIR)/build/stage-files.sh
	touch $(STARTWRT_OPENWRT)/files/.staged

# Full flashable image. Delegates the OpenWrt build to make -C openwrt; this is
# the multi-hour step. Resource fencing (memory-aware -j, cgroup) from the old
# standalone Makefile can be ported here if host stability requires it.
.PHONY: startwrt-image
startwrt-image: $(STARTWRT_IMAGES)
$(STARTWRT_IMAGES) &: $(STARTWRT_OPENWRT)/.config $(STARTWRT_OPENWRT)/files/.staged
	$(MAKE) -C $(STARTWRT_OPENWRT) V=s -j$(shell nproc)
	mkdir -p results
	cp $(STARTWRT_IMAGE_DIR)/$(STARTWRT_SDCARD_NAME) results/$(STARTWRT_SDCARD_NAME)
	cp $(STARTWRT_IMAGE_DIR)/$(STARTWRT_SYSUPGRADE_NAME) results/$(STARTWRT_SYSUPGRADE_NAME)

# Deploy binary + restart daemon over SSH (atomic temp -> sync -> rename).
.PHONY: startwrt-update
startwrt-update: $(STARTWRT_BIN)
	cat $(STARTWRT_BIN) | \
	ssh $(STARTWRT_REMOTE) 'set -e; \
		trap "rm -f /usr/bin/.startwrt.new" EXIT; \
		cat > /usr/bin/.startwrt.new; \
		chmod +x /usr/bin/.startwrt.new; \
		sync; \
		mv /usr/bin/.startwrt.new /usr/bin/startwrt; \
		ln -sf startwrt /usr/bin/startwrt-ctrld; \
		ln -sf startwrt /usr/bin/startwrt-cli; \
		/etc/init.d/startwrt restart'

.PHONY: clean-startwrt
clean-startwrt:
	rm -f results/$(STARTWRT_SDCARD_NAME) results/$(STARTWRT_SYSUPGRADE_NAME)
	rm -rf $(STARTWRT_DIR)/web/dist $(STARTWRT_DIR)/web/.angular
	rm -rf $(STARTWRT_OPENWRT)/files
	rm -rf $(STARTWRT_OPENWRT)/build_dir $(STARTWRT_OPENWRT)/staging_dir $(STARTWRT_OPENWRT)/tmp $(STARTWRT_OPENWRT)/bin
	rm -f $(STARTWRT_OPENWRT)/.config

# The web app is standalone in Stage A (its own prettier config); this is the
# Rust crate. (When the web joins the root Angular workspace in Stage B, fold
# its formatting into `format-web`.)
.PHONY: format-startwrt format-check-startwrt
format-startwrt:
	cargo +nightly fmt -p startwrt-core -p uciedit -p uciedit_macros
	npm --prefix $(STARTWRT_DIR)/web run format 2>/dev/null || true

format-check-startwrt:
	cargo +nightly fmt --check -p startwrt-core -p uciedit -p uciedit_macros
