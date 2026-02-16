# Contributing

## Project Structure

StartWRT is an OpenWrt fork with two main codebases:

- **`backend/`** — Rust workspace: RPC server, CLI, and UCI config library
- **`web/`** — Angular 21 frontend with Taiga UI v5

See each directory's CONTRIBUTING.md for stack-specific setup.

## Getting Started

### Backend

```bash
cd backend
cargo build                          # Build all crates
cargo build -p startwrt-ctrl         # Build ctrl only
cargo test -p uciedit                # Run UCI parser tests
```

### Frontend

```bash
cd web
npm ci                               # Install dependencies
npm start                            # Dev server with mock API
npm run build                        # Production build
npm run check                        # Type-check without emitting
```

The frontend dev server uses mocks by default (`config.json` → `useMocks: true`), so no router or running backend is needed during frontend development.

### Building OpenWrt

Use [docker-openwrt-build-env](https://github.com/mwarning/docker-openwrt-build-env). Native builds on some distros (e.g., Arch) produce silent errors.

## Architecture & Patterns

See [CLAUDE.md](CLAUDE.md) for the full-stack architecture overview, or the stack-specific guides:

- [backend/CLAUDE.md](backend/CLAUDE.md) — Backend modules, UCI library, Rust conventions
- [web/CLAUDE.md](web/CLAUDE.md) — Frontend patterns, Taiga UI, component conventions

## Key Documents

- [API_CONTRACT.md](API_CONTRACT.md) — Complete RPC endpoint contract with Rust types
