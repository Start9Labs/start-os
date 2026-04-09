# Contributing to StartWRT

For project structure and system architecture, see [ARCHITECTURE.md](ARCHITECTURE.md).

## Project Structure

StartWRT is an OpenWrt fork with two main codebases:

- **`backend/`** — Rust workspace: RPC server, CLI, and UCI config library
- **`web/`** — Angular 21 frontend with Taiga UI v5
- **`openwrt/`** — OpenWrt fork (git submodule)

See each directory's CONTRIBUTING.md for stack-specific setup.

## Prerequisites

- [Rust](https://rustup.rs/) (stable)
- [Node.js](https://nodejs.org/) (v22+)
- [Docker](https://docs.docker.com/get-docker/) (recommended for OpenWrt builds)

## Getting Started

### Frontend

```bash
cd web
npm ci                               # Install dependencies
npm start                            # Dev server with mock API
npm run build                        # Production build
npm run check                        # Type-check without emitting
```

The dev server uses mocks by default (`config.json` → `useMocks: true`), so no router or running backend is needed during frontend development.

### Backend

```bash
cd backend
cargo build                          # Build all crates
cargo build -p startwrt-ctrl         # Build ctrl only
cargo test -p uciedit                # Run UCI parser tests
```

For dev authentication, set `STARTWRT_DEV_PASSWORD` to bypass `/etc/shadow`.

### Building the Full Image

Use [docker-openwrt-build-env](https://github.com/mwarning/docker-openwrt-build-env) for consistent builds. Native builds on some distros produce silent errors.

```bash
make openwrt-setup                   # One-time: configure feeds, download packages
make                                 # Full build → out/*.img
```

### Make Targets

| Target | Description |
|--------|-------------|
| `make` / `make image` | Full build: web → Rust → stage → OpenWrt image |
| `make openwrt-setup` | One-time: configure feeds and download packages |
| `make image-quick` | Reimage without recompiling packages |
| `make update REMOTE=root@IP` | Deploy binary over SSH (default: root@192.168.0.1) |
| `make clean` | Delete all build artifacts |

The Makefile auto-derives parallel job count from available memory (3 GB per job budget), applies nice/ionice for low priority, and optionally uses systemd cgroup fencing.

### Deploying to a Device

```bash
make update                          # Deploy to default (192.168.0.1)
make update REMOTE=root@10.0.0.1     # Deploy to custom IP
```

Deployment is atomic: binary is written to a temp file, synced, then renamed into place. The daemon is restarted via init script. The web UI is embedded in the binary, so deploying the binary updates everything.

## Key Documents

- [ARCHITECTURE.md](ARCHITECTURE.md) — System architecture, data flow, build pipeline
- [API_CONTRACT.md](API_CONTRACT.md) — Complete RPC endpoint contract with Rust types
- [backend/ARCHITECTURE.md](backend/ARCHITECTURE.md) — Backend modules, UCI library, error types
- [web/ARCHITECTURE.md](web/ARCHITECTURE.md) — Frontend patterns, styling, component conventions
- [docs/init-reflash.md](docs/init-reflash.md) — Manufacturing, setup, and reflash specification
