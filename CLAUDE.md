# CLAUDE.md

For AI developers (Claude Code, Copilot, etc.). See `CONTRIBUTING.md` for the doc map and contribution workflow.

## Operating rules

- `openwrt/` is a git submodule pointing to Start9's OpenWrt fork. Clone with `--recursive` (or `git submodule update --init --recursive`). Don't edit files inside it; changes belong upstream.
- Don't run `make` (full image build) unsolicited — it builds OpenWrt from source and takes hours. For backend-only work use `cargo build` in `backend/`; for frontend-only work use `npm start` in `web/`. Use `make update REMOTE=...` only when explicitly asked to deploy.
- Read the component-level CLAUDE.md before operating on that component (`backend/CLAUDE.md`, `web/CLAUDE.md`) — they document footguns specific to each tree.
- Cross-frontend/backend changes: update `API_CONTRACT.md`, the Rust handler, `web/src/app/services/api/api.service.ts`, and both `live-api.service.ts` and `mock-api.service.ts` together. Skipping any of these breaks the contract.

## Sub-scopes

- `backend/CLAUDE.md` — Rust workspace (ctrl, uciedit, uciedit_macros)
- `web/CLAUDE.md` — Angular 21 + Taiga UI v5 frontend
