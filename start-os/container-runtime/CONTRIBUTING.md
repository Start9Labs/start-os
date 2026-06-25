# Contributing to Container Runtime

For general environment setup (Node, repo clone), see the root [CONTRIBUTING.md](../CONTRIBUTING.md). This file covers only what's specific to the container runtime.

## Documentation

This sub-tree's docs split across four files:

- `README.md` — what this is
- `ARCHITECTURE.md` — how it's built
- `CONTRIBUTING.md` — this file; how to contribute
- `CLAUDE.md` — AI-developer operating rules

**These docs must be kept up to date.** When you change the runtime's structure, conventions, build process, or RPC surface, update the relevant file(s) in the same change — do not defer.

## Prerequisites

- Node.js v20+ (see root CONTRIBUTING for the recommended version pin)
- The local SDK build at `../sdk/dist/` — `package.json` references `@start9labs/start-sdk` via `file:../sdk/dist`, so you must run `cd ../sdk && make baseDist dist` first whenever the SDK has changed.

## Common commands

```bash
npm ci                                   # install deps
npm run check                            # type-check (tsc --noEmit)
npm run build                            # prettier + clean + tsc to dist/
npm test                                 # jest
make -C .. test-container-runtime        # run the same tests via the workspace target
```

## Style

Prettier config lives in `package.json`:

- `trailingComma: "all"`, `tabWidth: 2`, `semi: false`, `singleQuote: false` (note: the runtime uses double quotes, unlike the SDK and web).

`npm run build` runs Prettier with `--write` before compiling, so formatting fixes happen automatically as part of a build.

## Tests

Jest with `ts-jest`. Config: `jest.config.js`. Place tests next to the code they cover or under a `test/` directory; use the `.test.ts` extension.

## Branch / commit / PR

Follow the conventions in the root [CONTRIBUTING.md](../CONTRIBUTING.md#commit-messages) — Conventional Commits with the `container-runtime` scope, e.g. `fix(container-runtime): ...`.
