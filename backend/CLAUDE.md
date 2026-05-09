# CLAUDE.md

## Operating rules

- New handler modules must export `pub fn <name><C: CtrlContext>() -> ParentHandler<C>` and be registered in `main_api()` in `ctrl/src/lib.rs`. Skipping the registration is silent — the endpoint won't exist.
- After UCI writes, call `/etc/init.d/<service> reload` — but only when `ctx.effectful()` is true. Reloading unconditionally breaks `--configs-only` CLI mode.
- Use `uciedit`'s retry mechanism for writes that may conflict with concurrent writes (profile creation already retries 4 times). Don't add ad-hoc retry loops.
- The generic `uci.get` / `uci.set` / `file.*` / `exec` endpoints are vestigial — every feature has a purpose-built smart endpoint and no frontend code calls the generics. Don't reach for them when adding new functionality; add a typed handler instead. See `../API_CONTRACT.md` for the full contract.
- For dev authentication, set `STARTWRT_DEV_PASSWORD` to bypass `/etc/shadow`. Don't try to populate `/etc/shadow` on a dev machine.
- Cross-frontend changes: when adding/modifying a handler, update `web/src/app/services/api/api.service.ts` and both `live-api.service.ts` / `mock-api.service.ts`, plus `../API_CONTRACT.md`.
