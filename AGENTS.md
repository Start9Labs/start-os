# CLAUDE.md

For AI developers (Claude Code, Copilot, etc.). See `CONTRIBUTING.md` for the doc map and contribution workflow.

## Operating rules

- **Polyglot repo.** Per-language gotchas live in component-level `CLAUDE.md` files — read the relevant one before operating on that component.
- **Verify cross-layer changes in order.** Rust → ts-bindings → SDK rebuild → web/container-runtime type checks. See [ARCHITECTURE.md](ARCHITECTURE.md#cross-layer-verification). Editing `sdk/base/lib/osBindings/*.ts` alone is NOT sufficient — the SDK bundle must be rebuilt before web/container-runtime see the change.
- **Ask before destructive `make` recipes.** Image flashing, deploy targets (`update*`, `reflash`, `wormhole*`), and `make clean*` consume hours and disk — confirm with the user first.
- **Use `make` recipes when they exist** rather than re-deriving the underlying commands.

## Sub-scopes

- `core/CLAUDE.md` — Rust backend
- `web/CLAUDE.md` — Angular 22 + Taiga UI 5 frontend
- `container-runtime/CLAUDE.md` — Node.js LXC service runtime
- `sdk/CLAUDE.md` — TypeScript service-packaging SDK
- `patch-db/CLAUDE.md` — git submodule; edits belong upstream
