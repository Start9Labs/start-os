# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for the full system architecture, project structure, data flow, build pipeline, and design decisions.

Each major component has its own docs: [backend/](backend/), [web/](web/).

## Build & Development

See [CONTRIBUTING.md](CONTRIBUTING.md) for environment setup, build commands, and development workflow.

**Quick reference:**

```bash
# Frontend
cd web && npm ci && npm start        # Dev server with mocks

# Backend
cd backend && cargo build            # Build all crates
cd backend && cargo test -p uciedit  # Run UCI parser tests

# Full image
make                                 # Build everything → out/*.img
make update REMOTE=root@192.168.0.1  # Deploy binary over SSH
```

## Operating Rules

- Always read the component-level CLAUDE.md before operating on that component: [backend/CLAUDE.md](backend/CLAUDE.md), [web/CLAUDE.md](web/CLAUDE.md)
- Follow existing patterns before inventing new ones
- Check [API_CONTRACT.md](API_CONTRACT.md) for the RPC endpoint contract when working across frontend/backend

## Supplementary Documentation

- `docs/TODO.md` — Pending tasks
- `docs/init-reflash.md` — Manufacturing, setup wizard, and reflash flow specification
