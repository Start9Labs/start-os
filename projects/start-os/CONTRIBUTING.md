# Contributing to the StartOS OS product

This guide covers building and contributing to the **StartOS OS product** in `projects/start-os/` — the `startbox` / `start-container` bins, the web UIs, the container runtime, and the bootable OS image. It is the source of truth for the OS-image toolchain and the build/deploy targets.

Start from the root [CONTRIBUTING.md](../../CONTRIBUTING.md) for the shared toolchain (Rust, Node 24, Docker, Make, git), branch policy, and the repo-wide commit/PR conventions; this file adds the StartOS-specific setup on top.

If you want to **package a service** for StartOS instead, see the [packaging guide](https://docs.start9.com/packaging). For other ways to help, see [start9.com/contribute](https://start9.com/contribute).

## Documentation

User-facing changes (UI, CLI, install/setup flow) must update the end-user docs under `docs/` (an mdbook served at `/start-os/`) in the same change. This product's docs: [README.md](README.md) (what it is / usage), [ARCHITECTURE.md](ARCHITECTURE.md) (how it's wired), this file (how to build & contribute), and [AGENTS.md](AGENTS.md) (agent rules; `CLAUDE.md` is a one-line `@AGENTS.md` import).

## Collaboration

- [Matrix](https://matrix.to/#/#dev-startos:matrix.start9labs.com)
- Security issues: [security@start9.com](mailto:security@start9.com)

## Prerequisites

The OS product is a thin wrapper over the shared `start-core` crate (`shared-libs/crates/start-core`), the shared TypeScript modules (`shared-libs/ts-modules`), and the SDK (`projects/start-sdk`). Build commands run from the **repo root** unless noted; the product dir is `projects/start-os`.

If you're only working on the admin UI or setup-wizard, you don't need the OS-image toolchain below — the web apps build and run standalone against mock data. See [`shared-libs/ts-modules/CONTRIBUTING.md`](../../shared-libs/ts-modules/CONTRIBUTING.md).

Beyond the shared toolchain in the [root CONTRIBUTING](../../CONTRIBUTING.md#environment-setup), **building the OS image needs multi-arch emulation and image-packaging tools** (Debian/Ubuntu):

```sh
sudo apt install -y qemu-user-static binfmt-support squashfs-tools debspawn b3sum
sudo mkdir -p /etc/debspawn/
echo "AllowUnsafePermissions=true" | sudo tee /etc/debspawn/global.toml

# Register cross-arch binfmt handlers and a buildx builder (one-time)
docker run --privileged --rm tonistiigi/binfmt --install all
docker buildx create --use
```

### Development Mode

For faster iteration during development:

```sh
. ./devmode.sh
```

This sets `ENVIRONMENT=dev` and `GIT_BRANCH_AS_HASH=1` to prevent rebuilds on every commit.

## Build configuration

OS builds are parameterized by environment variables:

| Variable             | Description                                                                                          |
| -------------------- | --------------------------------------------------------------------------------------------------- |
| `PLATFORM`           | Target platform: `x86_64`, `x86_64-nonfree`, `aarch64`, `aarch64-nonfree`, `riscv64`, `raspberrypi` |
| `ENVIRONMENT`        | Hyphen-separated feature flags (see below)                                                           |
| `PROFILE`            | Build profile: `release` (default) or `dev`                                                          |
| `GIT_BRANCH_AS_HASH` | Set to `1` to use the git branch name as the version hash (avoids rebuilds)                          |

**ENVIRONMENT flags:**

- `dev` — enables password SSH before setup, skips frontend compression
- `unstable` — enables assertions and debugging with a performance penalty
- `console` — enables tokio-console for async debugging

**Platform notes:**

- `-nonfree` variants include proprietary firmware and drivers
- `raspberrypi` includes non-free components by necessity
- Platform is remembered between builds if not specified

## Building

The web UIs are embedded into `startbox` at compile time (`include_dir!`), so the web build must precede the Rust build — always go through the `Makefile`, which encodes the ordering. For faster web iteration use `npm run start:ui` (see [`shared-libs/ts-modules/CONTRIBUTING.md`](../../shared-libs/ts-modules/CONTRIBUTING.md)).

```sh
cargo check -p start-os        # verify the OS bins compile (startbox, start-container)
make startos-ui                # build the admin UI (startos-uis for ui + setup-wizard)
make startos                   # build all OS artifacts (bins + web + container-runtime image)
make startos-$(IMAGE_TYPE)     # build the bootable image (startos-iso; startos-img on Raspberry Pi)
make startos-deb               # Debian package (startos-squashfs for the squashfs image)
```

`make ts-bindings` regenerates the TS bindings from the Rust types (see [Cross-layer changes](#cross-layer-changes)).

### Deploying to a device

These targets push to a **live device** and are slow/destructive — be deliberate. For devices on the same network:

| Target                                       | Description                                     |
| -------------------------------------------- | ----------------------------------------------- |
| `startos-update-startbox REMOTE=start9@<ip>` | Deploy binary + UI only (fastest)               |
| `startos-update-deb REMOTE=start9@<ip>`      | Deploy full Debian package                      |
| `startos-update REMOTE=start9@<ip>`          | OTA-style update                                |
| `startos-emulate-reflash REMOTE=start9@<ip>` | Reflash as if using a live ISO                  |
| `startos-update-overlay REMOTE=start9@<ip>`  | Deploy to in-memory overlay (reverts on reboot) |

For devices on a different network (uses [magic-wormhole](https://github.com/magic-wormhole/magic-wormhole)):

| Target                      | Description               |
| --------------------------- | ------------------------- |
| `startos-wormhole`          | Send the startbox binary  |
| `startos-wormhole-deb`      | Send the Debian package   |
| `startos-wormhole-squashfs` | Send the squashfs image   |

### Creating a VM

Install virt-manager:

```sh
sudo apt install -y virt-manager
sudo usermod -aG libvirt $USER
sudo su $USER
virt-manager
```

Build an ISO first:

```sh
PLATFORM=$(uname -m) ENVIRONMENT=dev make startos-iso
```

Then follow the screenshot walkthrough in [`assets/create-vm/`](assets/create-vm/) to create a new virtual machine. Key steps:

1. Create a new virtual machine
2. Browse for the ISO — create a storage pool pointing to your `results/` directory
3. Select "Generic or unknown OS"
4. Set memory and CPUs
5. Create a disk and name the VM

## Testing

```sh
make test                      # Rust + SDK + container-runtime
make test-core                 # backend only
```

The container-runtime has its own test suite and prettier config (double quotes, no semicolons) — see [container-runtime/CONTRIBUTING.md](container-runtime/CONTRIBUTING.md). Note CI builds a multi-platform matrix (apple-darwin + aarch64/x86_64/riscv64 musl); local `cargo check` is linux-only, so consider platform-specific impact.

## Formatting

```sh
make format-startos            # format this product (core bins + web + container-runtime)
make format-check-startos      # CI-style check
```

## Cross-layer changes

When a change crosses Rust → bindings → SDK → web/runtime, verify in order:

1. `cargo check -p start-os`
2. `make ts-bindings` — regenerate ts-rs types from `start-core`
3. `cd projects/start-sdk && make bundle` — rebuild the SDK `dist` (builds `@start9labs/start-core` first and bundles it; required before the web apps / runtime can see new bindings)
4. `npm run check:ui && npm run check:setup`
5. `cd projects/start-os/container-runtime && npm run check`
