# Contributing to the StartOS OS product

This guide covers contributing to the **OS product** in `start-os/`. For
full environment setup (Debian/Ubuntu packages, Docker + binfmt, Rust, Node 24),
branch policy, and cross-product workflow, follow the root
[CONTRIBUTING.md](../../CONTRIBUTING.md) first — it is the source of truth for the
toolchain. This file adds product-specific build/test details.

If you want to **package a service** for StartOS instead, see the
[service packaging guide](https://github.com/Start9Labs/ai-service-packaging).
For other ways to help, see [start9.com/contribute](https://start9.com/contribute).

## Documentation

User-facing changes (UI, CLI, install/setup flow) must update the end-user docs
under `docs/` (an mdbook served at `/start-os/`) in the same change. This
product's docs: [README.md](README.md) (what it is / usage),
[ARCHITECTURE.md](ARCHITECTURE.md) (how it's wired), this file (how to build &
contribute), and [AGENTS.md](AGENTS.md) (agent rules; `CLAUDE.md` is a one-line
`@AGENTS.md` import).

## Collaboration

- [Matrix](https://matrix.to/#/#dev-startos:matrix.start9labs.com)
- Security issues: [security@start9.com](mailto:security@start9.com)

## Prerequisites

This is a monorepo. The OS product is a thin wrapper over the shared
`start-core` crate (`shared-libs/crates/start-core`), the shared TypeScript
modules (`shared-libs/ts-modules`), and the SDK (`projects/start-sdk`). Build commands run from the **repo
root** unless noted.

Clone and target the right integration branch (`master` for the
current release; `next/patch`, `next/minor`, `next/major` otherwise — ask a
maintainer if unsure):

```sh
git clone https://github.com/Start9Labs/start-technologies.git
cd projects/start-os
```

## Building

```sh
cargo check -p start-os        # verify the OS bins compile (startbox, start-container)
make startos-ui                # build the admin UI
make startos-uis               # build ui + setup-wizard
make startos                   # build all OS artifacts (bins + web + container-runtime image)
make startos-$(IMAGE_TYPE)     # build the bootable image (iso; img on Raspberry Pi)
make startos-deb / make startos-squashfs   # package outputs
```

The web UIs are embedded into `startbox` at compile time, so the web build must
precede the Rust build — always go through the `Makefile`, which encodes the
ordering. For faster iteration use `./devmode.sh` / dev mode (see root
CONTRIBUTING) and `npm run start:ui`.

Deploy/flash targets (`startos-update*`, `startos-wormhole*`, `startos-emulate-reflash`)
push to a live device and are slow/destructive — be deliberate.

## Testing

```sh
make test                      # Rust + SDK + container-runtime
make test-core                 # backend only
```

The container-runtime has its own test suite and prettier config (double quotes,
no semicolons) — see [container-runtime/CONTRIBUTING.md](container-runtime/CONTRIBUTING.md).
Note CI builds a multi-platform matrix (apple-darwin + aarch64/x86_64/riscv64
musl); local `cargo check` is linux-only, so consider platform-specific impact.

## Formatting

```sh
make format-startos            # format this product (core bins + web + container-runtime)
make format-check-startos      # CI-style check
```

## Cross-layer changes

When a change crosses Rust → bindings → SDK → web/runtime, verify in order:

1. `cargo check -p start-os`
2. `make ts-bindings` — regenerate ts-rs types from `start-core`
3. `cd projects/start-sdk && make bundle` — rebuild the SDK `dist` (builds `@start9labs/start-core`
   first and bundles it; required before the web apps / runtime can see new bindings)
4. `npm run check:ui && npm run check:setup`
5. `cd projects/start-os/container-runtime && npm run check`
