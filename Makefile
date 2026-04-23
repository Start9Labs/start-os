ARCH := riscv64
RUST_ARCH := riscv64gc
PROFILE := release

# ---------------------------------------------------------------------------
# Build parallelism
#
# Upstream OpenWrt relies on the user to supply -j; the build system
# propagates the job count through GNU Make's jobserver to all sub-makes.
# This wrapper auto-derives a conservative job count from currently
# *available* memory so `make` works safely without extra flags.
#
#   JOBS          — min(nproc/2, MemAvailable / 3 GB per job), minimum 1
#   nice/ionice   — low scheduling priority so the host stays responsive
#   --load-average— standard Make back-pressure under heavy system load
#   oom_score_adj — makes build the preferred OOM-kill target (not session)
#   cgroup fence  — hard memory ceiling via systemd-run (when available)
#
# Override: make JOBS=4
# ---------------------------------------------------------------------------

# Derive JOBS from *available* memory (not total RAM) — MemAvailable already
# accounts for memory used by other processes, kernel caches, etc.
# Budget 3 GB per parallel job to cover worst-case GCC/kernel link phases
# (kernel, samba4, ffmpeg links can peak at 3-4 GB each).
# Capped at nproc/2 so the host desktop/session always has spare cores.
JOBS ?= $(shell \
  cpus=$$(( $$(nproc) / 2 )); \
  [ $$cpus -lt 1 ] && cpus=1; \
  avail_kb=$$(awk '/MemAvailable/{print $$2}' /proc/meminfo); \
  mem_jobs=$$(( avail_kb / 3145728 )); \
  [ $$mem_jobs -lt 1 ] && mem_jobs=1; \
  j=$$(( cpus < mem_jobs ? cpus : mem_jobs )); \
  [ $$j -lt 1 ] && j=1; \
  echo $$j)

# Run builds at low CPU/IO priority so the host stays responsive.
NICE := nice -n 19 ionice -c 3

# Standard Make back-pressure: refuse to start new jobs when system load
# exceeds JOBS + 2.
LOAD_LIMIT := --load-average=$(shell echo $$(( $(JOBS) + 2 )))

# Raise the OOM killer score so the build is killed before the user session.
# Range: -1000 to 1000; higher = more likely to be killed.  Default is 0;
# 500 makes build processes strongly preferred OOM targets.
OOM_ADJ := echo 500 > /proc/self/oom_score_adj 2>/dev/null;

# Cgroup memory fence via systemd-run (best-effort).
# MemoryMax caps the build's total memory so it gets killed before the host
# session starves.  Budget: JOBS * 3 GB + 1 GB headroom for tooling overhead.
# Falls back to just oom_score_adj if systemd-run is unavailable.
MEMLIMIT_BYTES := $(shell echo $$(( $(JOBS) * 3221225472 + 1073741824 )))
HAS_SYSTEMD_RUN := $(shell systemd-run --user --scope true 2>/dev/null && echo 1 || echo 0)
ifeq ($(HAS_SYSTEMD_RUN),1)
  CGROUP_WRAP := systemd-run --user --scope -p MemoryMax=$(MEMLIMIT_BYTES) --
else
  CGROUP_WRAP :=
  $(warning systemd-run not available — no cgroup memory fence; relying on oom_score_adj only)
endif

# Limit cargo parallelism to match JOBS
export CARGO_BUILD_JOBS := $(JOBS)
export ARCH RUST_ARCH PROFILE

# Build-time env stamps (regenerated on git-state change)
GIT_HASH_FILE := $(shell ./build/env/check-git-hash.sh)

# Source tracking for incremental builds
RUST_SRC := $(shell git ls-files backend/ctrl/ backend/uciedit/ backend/uciedit_macros/) backend/Cargo.toml backend/Cargo.lock
WEB_SRC := $(shell git ls-files web/src/)
CONFIG_SRC := $(shell git ls-files backend/firstboot_config/)
BUILD_SCRIPTS := $(shell git ls-files build/)

# Outputs
RUST_BIN_DIR := backend/target/$(RUST_ARCH)-unknown-linux-musl/$(PROFILE)
RUST_BIN := $(RUST_BIN_DIR)/startwrt
WEB_DIST := web/dist/startwrt/browser/index.html
OPENWRT_IMAGE_NAME := openwrt-spacemit-k1-sbc-bananapi-f3-squashfs-pack-sdcard.img
OPENWRT_IMAGE_SRC := openwrt/bin/targets/spacemit/$(OPENWRT_IMAGE_NAME)
OPENWRT_IMAGE := out/$(OPENWRT_IMAGE_NAME)

# ---------------------------------------------------------------------------
# Remote deployment (SSH transport)
#
# Requires SSH key auth: ssh-copy-id root@<ip>
#
# Usage:  make update
#         make update REMOTE=root@10.0.0.1   # override target device
# ---------------------------------------------------------------------------
REMOTE ?= root@192.168.0.1
PV := $(shell command -v pv 2>/dev/null || echo cat)

.PHONY: all clean openwrt-setup stage image image-quick update
.DELETE_ON_ERROR:

all: image

# One-time OpenWrt setup (feeds, config, download)
openwrt-setup: openwrt/.config

openwrt/.config: build/openwrt.diffconfig build/feeds.conf openwrt/.git
	./build/openwrt-setup.sh

$(RUST_BIN): $(RUST_SRC) $(WEB_DIST) build/build-rust.sh
	$(OOM_ADJ) $(CGROUP_WRAP) $(NICE) ./build/build-rust.sh
	@touch $(RUST_BIN)

$(WEB_DIST): $(WEB_SRC) web/package-lock.json web/config.json
	$(NICE) npm --prefix web install
	$(OOM_ADJ) $(CGROUP_WRAP) $(NICE) npm --prefix web run build

# Prod config: generated from config-sample.json with useMocks=false
# and gitHash stamped from build/env/GIT_HASH.txt.
web/config.json: $(GIT_HASH_FILE) web/config-sample.json web/update-config.sh
	./web/update-config.sh

# Stage custom files into openwrt/files/
stage: openwrt/files/.staged

openwrt/files/.staged: $(RUST_BIN) $(CONFIG_SRC) build/stage-files.sh
	./build/stage-files.sh
	touch openwrt/files/.staged

# Build OpenWrt image (depends on staging)
image: $(OPENWRT_IMAGE)

$(OPENWRT_IMAGE): openwrt/.config openwrt/files/.staged
	@printf '  Build resources: JOBS=%s  MemAvail=%s MB  MemMax=%s MB  cgroup=%s\n' \
		'$(JOBS)' \
		"$$(awk '/MemAvailable/{printf "%d", $$2/1024}' /proc/meminfo)" \
		'$(shell echo $$(( $(MEMLIMIT_BYTES) / 1048576 )))' \
		'$(if $(filter 1,$(HAS_SYSTEMD_RUN)),yes,no)'
	$(OOM_ADJ) $(CGROUP_WRAP) $(NICE) $(MAKE) -C openwrt V=s -j$(JOBS) $(LOAD_LIMIT)
	mkdir -p out
	cp $(OPENWRT_IMAGE_SRC) $(OPENWRT_IMAGE)

# Clean
# backend/target may contain root-owned files when a Docker build is
# interrupted before build-rust.sh's chown trap fires; in that case delegate
# removal to the same cargo-zigbuild container so root can rm its own files.
clean:
	rm -rf out
	@if [ -d backend/target ] && find backend/target -not -user "$$(id -u)" -print -quit | grep -q .; then \
	  echo "  backend/target has root-owned files — removing via docker"; \
	  docker run --rm -v "$$(pwd)":/workdir -w /workdir start9/cargo-zigbuild rm -rf backend/target; \
	else \
	  rm -rf backend/target; \
	fi
	rm -rf web/dist web/.angular
	rm -rf openwrt/files
	rm -rf openwrt/build_dir openwrt/staging_dir openwrt/tmp openwrt/bin
	rm -f openwrt/.config

# Rebuild just the image (after changing staged files, skip recompiling packages)
image-quick: openwrt/files/.staged
	$(OOM_ADJ) $(CGROUP_WRAP) $(NICE) $(MAKE) -C openwrt target/install V=s -j$(JOBS) $(LOAD_LIMIT)

# ---------------------------------------------------------------------------
# Rapid deployment targets — atomic, all-or-nothing
#
# Each target pipes a tarball over a single SSH connection.  On the remote
# side files are extracted to a temp location, cp'd next to their targets
# (same filesystem ⇒ rename is atomic), synced, then mv'd into place.
# A trap on EXIT cleans up .new artifacts if anything fails mid-deploy.
# ---------------------------------------------------------------------------

# Deploy binary + restart daemon (web UI is embedded in binary)
update: $(RUST_BIN)
	$(PV) $(RUST_BIN_DIR)/startwrt | \
	ssh $(REMOTE) 'set -e; \
		trap "rm -f /usr/bin/.startwrt.new" EXIT; \
		cat > /usr/bin/.startwrt.new; \
		chmod +x /usr/bin/.startwrt.new; \
		sync; \
		mv /usr/bin/.startwrt.new /usr/bin/startwrt; \
		ln -sf startwrt /usr/bin/startwrt-ctrld; \
		ln -sf startwrt /usr/bin/startwrt-cli; \
		/etc/init.d/startwrt restart'
