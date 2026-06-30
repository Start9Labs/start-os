# Contributing to StartWRT

Build/test/format and commit/PR conventions that apply repo-wide live in the root
[`CONTRIBUTING.md`](../../CONTRIBUTING.md); this file covers only what is specific to start-wrt.
For structure and data flow see [ARCHITECTURE.md](ARCHITECTURE.md); for the RPC contract see
[API_CONTRACT.md](API_CONTRACT.md).

All commands below run from the **repo root**.

## Backend (Rust)

The three crates (`startwrt-core`/`ctrl`, `uciedit`, `uciedit_macros`) are members of the root
Cargo workspace.

```bash
cargo build -p startwrt-core --bin startwrt                     # host build of the daemon+CLI binary
cargo check -p startwrt-core --bin startwrt                     # fast type-check
cargo test  -p startwrt-core -p uciedit -p uciedit_macros       # all start-wrt unit tests
make test-startwrt                                              # same tests, containerized (mirrors test-core)
```

> **Always scope `cargo test` with `-p`.** A bare `cargo test` (or `cargo test` run from
> `backend/`) now tests the *entire* monorepo workspace — including `startos-backup-fs`, whose
> `fuser` dependency needs FUSE dev libs that exist only in the build container, so it fails on a
> bare host. start-wrt's own crates are fuser-free, so the `-p`-scoped command above runs cleanly
> on the host. `make test-startwrt` runs the same scoped set inside `start9/cargo-zigbuild`.

`startwrt-core` depends on the shared `start-core` crate (aliased as `startos`), plus the
vendored `rpc-toolkit` and `imbl-value`. For dev authentication set `STARTWRT_DEV_PASSWORD` to
bypass `/etc/shadow`.

> The host build embeds the web UI via `include_dir!`, so it needs `projects/start-wrt/web/dist/`
> to exist — run the web build first (below), or build the full binary with `make startwrt`.

## Frontend (Angular, standalone in this stage)

The web app keeps its own `package.json`/`node_modules` and is **not** part of the root Angular
workspace yet (that's a later migration stage). Use `--prefix`:

```bash
npm --prefix projects/start-wrt/web ci         # install
npm --prefix projects/start-wrt/web start      # dev server (mock API, no backend needed)
npm --prefix projects/start-wrt/web run build  # production build → web/dist/
npm --prefix projects/start-wrt/web run check  # type-check
```

## Building / deploying (via the root Makefile)

start-wrt's targets live in [`build.mk`](build.mk) (included by the root `Makefile`):

| Target | Description |
|--------|-------------|
| `make startwrt` | web → riscv64 binary (cross-compiled via dockerized cargo-zigbuild) |
| `make startwrt-openwrt-setup` | one-time: openwrt feeds/config/download (needs the `openwrt` submodule) |
| `make startwrt-image` | full flashable OpenWrt image → `results/` (**hours**) |
| `make startwrt-update REMOTE=root@IP` | deploy binary over SSH (default `root@192.168.0.1`) |
| `make clean-startwrt` | remove start-wrt build artifacts |

Deployment is atomic (temp file → sync → rename → daemon restart). The web UI is embedded in
the binary, so deploying the binary updates everything.

> **⚠ UNVALIDATED since the monorepo migration.** The riscv dockerized cross-build (`make
> startwrt`) and the OpenWrt image assembly (`make startwrt-image`) have not yet been run on a
> build host. The backend host `cargo check` passes. Validate `make startwrt` first (it does not
> need the multi-GB `openwrt` submodule), then `make startwrt-image`. The OpenWrt build needs a
> consistent environment — Docker is recommended; native builds on some distros fail silently.

## Coupled changes

When you change a build input in `build.mk`, mirror it into the `paths:` filter of
`.github/workflows/start-wrt.yaml` (see root AGENTS.md "Coupled changes"). Cross-frontend/backend
changes must update `API_CONTRACT.md`, the Rust handler, and the web `api.service.ts` +
`live-api.service.ts` + `mock-api.service.ts` together.

## Key documents

- [ARCHITECTURE.md](ARCHITECTURE.md) — system architecture, data flow, build pipeline
- [API_CONTRACT.md](API_CONTRACT.md) — complete RPC endpoint contract with Rust types
- [backend/AGENTS.md](backend/AGENTS.md) / [web/AGENTS.md](web/AGENTS.md) — component rules
- [docs/init-reflash.md](docs/init-reflash.md) — manufacturing, setup, and reflash specification
