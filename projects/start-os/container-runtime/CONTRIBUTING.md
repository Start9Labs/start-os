# Contributing to Container Runtime

For general environment setup (Node, repo clone, monorepo layout), see the root [CONTRIBUTING.md](../../CONTRIBUTING.md). This file covers only what's specific to the container runtime.

## Documentation

This sub-tree's docs are:

- `README.md` — what this is + quickstart
- `ARCHITECTURE.md` — how it's structured (modules, RPC boundary, image build)
- `AGENTS.md` — agent/dev operating rules (imported by `CLAUDE.md`)
- `CONTRIBUTING.md` — this file
- `RPCSpec.md` — the JSON-RPC wire protocol

**Keep these up to date.** When you change the runtime's structure, conventions, build process, or RPC surface, update the relevant file(s) in the same change — don't defer.

## Prerequisites

- Node.js v20+ (see root CONTRIBUTING for the recommended version pin).
- The **built** SDK at `../../start-sdk/dist`. `package.json` references `@start9labs/start-sdk` via `file:../../start-sdk/dist`, so build the SDK whenever it changes:

  ```bash
  cd start-sdk && make bundle && cd -      # or: make baseDist dist
  ```

## Common commands

Run from the monorepo root:

```bash
npm --prefix start-os/container-runtime ci          # install deps
npm --prefix start-os/container-runtime run check     # type-check (tsc --noEmit)
npm --prefix start-os/container-runtime run build      # prettier + clean + tsc -> dist/
npm --prefix start-os/container-runtime test           # jest
make test-container-runtime                            # build SDK + run jest via Makefile
```

Or `cd start-os/container-runtime` first and drop the `--prefix`.

## Style

Prettier config lives in `package.json`:

- `trailingComma: "all"`, `tabWidth: 2`, `semi: false`, `singleQuote: false`.

The runtime uses **double quotes**, unlike `start-sdk` and `shared/web` (single quotes there) — do not normalize. `npm run build` runs Prettier `--write` before compiling, so formatting is applied automatically during a build.

## Tests

Jest with `ts-jest` (`jest.config.js`, `rootDir: ./src`). The `mime` module is mocked via `__mocks__/mime.js`. Place tests next to the code they cover with the `.test.ts` extension. `SystemForEmbassy` uses snapshot tests (`__snapshots__/`) and fixtures (`__fixtures__/`) — update snapshots deliberately, not blindly.

## Branch / commit / PR

Follow the conventions in the root [CONTRIBUTING.md](../../CONTRIBUTING.md#commit-messages) — Conventional Commits with the `container-runtime` scope, e.g. `fix(container-runtime): ...`.
