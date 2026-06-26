# AGENTS.md — Container Runtime

Node.js/TypeScript service runtime that runs inside StartOS package LXC containers. Sub-component of the **start-os** product. Wire-protocol reference: [RPCSpec.md](RPCSpec.md); structure: [ARCHITECTURE.md](ARCHITECTURE.md).

## Operating rules

- **Depends on the _built_ SDK at `../../start-sdk/dist`** (declared in `package.json` as `"@start9labs/start-sdk": "file:../../start-sdk/dist"`). Editing `start-sdk/` source alone has no effect here — rebuild the SDK first: `cd projects/start-sdk && make bundle` (or `make baseDist dist`). The Makefile target `start-os/container-runtime/package-lock.json` also depends on `start-sdk/dist/package.json`, so a stale SDK can break `npm ci`/`check`/`test`.
- **Style: double quotes, no semicolons.** Prettier config lives in `package.json` (`semi: false`, `singleQuote: false`, `trailingComma: "all"`, `tabWidth: 2`). This differs from `start-sdk` / `shared-libs/ts-modules` (single quotes there) — do NOT "normalize" to single quotes. `npm run build` runs Prettier `--write` before compiling.
- **`CLAUDE.md` is just `@AGENTS.md`** — edit this file, not `CLAUDE.md`.

## Build / test

Run from the monorepo root (`/path/to/start-os`):

```bash
cd projects/start-sdk && make bundle && cd -                  # build SDK dependency first
npm --prefix projects/start-os/container-runtime ci           # install deps
npm --prefix projects/start-os/container-runtime run check     # tsc --noEmit (type-check)
npm --prefix projects/start-os/container-runtime run build      # prettier + clean + tsc -> dist/
npm --prefix projects/start-os/container-runtime test           # jest (ts-jest)
make test-container-runtime                            # SDK + jest via top-level Makefile
```

Tests are Jest + `ts-jest` (`jest.config.js`, `rootDir: ./src`). `mime` is mocked via `__mocks__/mime.js`. Place tests next to the code as `*.test.ts`; `SystemForEmbassy` carries snapshot tests (`__snapshots__/`) and fixtures (`__fixtures__/`).

## Map

- `src/index.ts` — entry; wires `getSystem` into `RpcListener`.
- `src/Adapters/RpcListener.ts` — JSON-RPC dispatch on `service.sock`.
- `src/Adapters/EffectCreator.ts` — builds effects passed to procedures.
- `src/Adapters/Systems/` — `SystemForStartOs` (current) + `SystemForEmbassy` (legacy compat: manifest/config-spec transforms, effect polyfills).
- `src/Interfaces/`, `src/Models/` — contracts and domain types.

## Image build (gotchas)

- Compiled JS is installed into the container at `/usr/lib/startos/init/index.js` (the systemd unit runs `start-container pipe-wrap node … /usr/lib/startos/init/index.js`).
- `update-image-local.sh` mounts the **repo root** into `start9/build-env` and runs `update-image.sh` inside it. Note: `update-image.sh` still copies `start-container` from a `../core/target/...` path — that is a stale pre-monorepo path (the binary now builds into the root `target/<arch>-unknown-linux-musl/release/`). Don't propagate `core/` as a current path elsewhere; flag/fix it in code only if asked.
- The squashfs lands at `rootfs.<arch>.squashfs` and is installed to `/usr/lib/startos/container-runtime/rootfs.squashfs`.

## Stale-path note (monorepo)

Pre-monorepo docs referenced `core/`, `sdk/`, `web/`, `patch-db/`, `container-runtime/` at the repo root. Current locations: host lib `shared-libs/crates/start-core`, SDK `start-sdk`, Angular `shared-libs/ts-modules` + product `web/` dirs, this runtime `start-os/container-runtime`, first-party `shared-libs/crates/patch-db`.
