# Contributing to Start SDK

This guide covers developing the SDK itself. If you're building a service package *using* the SDK, see the [packaging docs](https://docs.start9.com/packaging).

For contributing to the broader StartOS project, see the root [CONTRIBUTING.md](../CONTRIBUTING.md).

## Prerequisites

- **Node.js v22+** (via [nvm](https://github.com/nvm-sh/nvm) recommended)
- **npm** (ships with Node.js)
- **GNU Make**

Verify your setup:

```bash
node --version   # v22.x or higher
npm --version
make --version
```

## Repository Layout

```
sdk/
├── base/              # @start9labs/start-sdk-base (core types, ABI, effects)
│   ├── lib/           #   TypeScript source
│   ├── package.json
│   ├── tsconfig.json
│   └── jest.config.js
├── package/           # @start9labs/start-sdk (full developer-facing SDK)
│   ├── lib/           #   TypeScript source
│   ├── package.json
│   ├── tsconfig.json
│   └── jest.config.js
├── baseDist/          # Build output for base (generated)
├── dist/              # Build output for package (generated, published to npm)
├── Makefile           # Build orchestration
├── README.md
├── ARCHITECTURE.md
└── CLAUDE.md
```

## Getting Started

Install dependencies for both sub-packages:

```bash
cd sdk
make node_modules
```

This runs `npm ci` in both `base/` and `package/`.

## Building

### Full Build

```bash
make bundle
```

This runs the complete pipeline: TypeScript compilation, hand-written pair copying, node_modules bundling, formatting, and tests. Outputs land in `baseDist/` (base) and `dist/` (package).

### Individual Targets

| Target | Description |
|--------|-------------|
| `make bundle` | Full build: compile + format + test |
| `make baseDist` | Compile base package only |
| `make dist` | Compile full package (depends on base) |
| `make fmt` | Run Prettier on all `.ts` files |
| `make check` | Type-check without emitting (both packages) |
| `make clean` | Remove all build artifacts and node_modules |

### What the Build Does

1. **Peggy parser generation** — `base/lib/exver/exver.pegjs` is compiled to `exver.ts` (the ExVer version parser)
2. **TypeScript compilation** — Strict mode, CommonJS output, declaration files
   - `base/` compiles to `baseDist/`
   - `package/` compiles to `dist/`
3. **Hand-written pair copying** — `.js`/`.d.ts` files without a corresponding `.ts` source are copied into the output directories. These are manually maintained JavaScript files with hand-written type declarations.
4. **Dependency bundling** — `node_modules/` is rsynced into both output directories so the published package is self-contained
5. **Formatting** — Prettier formats all TypeScript source
6. **Testing** — Jest runs both test suites

## Testing

```bash
# Run all tests (base + package)
make test

# Run base tests only
make base/test

# Run package tests only
make package/test

# Run a specific test file directly
cd base && npx jest --testPathPattern=exver
cd package && npx jest --testPathPattern=host
```

Tests use [Jest](https://jestjs.io/) with [ts-jest](https://kulshekhar.github.io/ts-jest/) for TypeScript support. Configuration is in each sub-package's `jest.config.js`.

### Test Files

Tests live alongside their subjects or in dedicated `test/` directories:

- `base/lib/test/` — ExVer parsing, input spec types, deep merge, graph utilities, type validation
- `base/lib/util/inMs.test.ts` — Time conversion utility
- `package/lib/test/` — Health checks, host binding, input spec builder

Test files use the `.test.ts` extension and are excluded from compilation via `tsconfig.json`.

## Formatting

```bash
make fmt
```

Runs Prettier with the project config (single quotes, no semicolons, trailing commas, 2-space indent). The Prettier config lives in each sub-package's `package.json`:

```json
{
  "trailingComma": "all",
  "tabWidth": 2,
  "semi": false,
  "singleQuote": true
}
```

## Type Checking

To check types without building:

```bash
make check
```

Or directly per package:

```bash
cd base && npm run check
cd package && npm run check
```

Both packages use strict TypeScript (`"strict": true`) targeting ES2021 with CommonJS module output.

## Local Development with a Service Package

To test SDK changes against a local service package without publishing to npm:

```bash
# Build and create a local npm link
make link

# In your service package directory
npm link @start9labs/start-sdk
```

This symlinks the built `dist/` into your global node_modules so your service package picks up local changes.

## Publishing

```bash
make publish
```

This builds the full bundle, then runs `npm publish --access=public --tag=latest` from `dist/`. The published package is `@start9labs/start-sdk`.

Only the `dist/` directory is published — it contains the compiled JavaScript, declaration files, bundled dependencies, and package metadata.

## Adding New Features

### Base vs Package

Decide where new code belongs:

- **`base/`** — Types, interfaces, ABI contracts, OS bindings, and low-level builders that have no dependency on the package layer. Code here should be usable independently.
- **`package/`** — Developer-facing API, convenience wrappers, runtime helpers (daemons, health checks, backups, file helpers, subcontainers). Code here imports from base and adds higher-level abstractions.

### Key Conventions

- **Builder pattern** — Most APIs use immutable builder chains (`.addDaemon()`, `.mountVolume()`, `.addAction()`). Each call returns a new type that accumulates configuration.
- **Effects boundary** — All runtime interactions go through the `Effects` interface. Never call system APIs directly.
- **Manifest type threading** — The manifest type flows through generics so that volume names, image IDs, and dependency IDs are type-constrained.
- **Re-export from package** — If you add a new export to base, also re-export it from `package/lib/index.ts` (or expose it through `StartSdk.build()`).

### Adding OS Bindings

Types in `base/lib/osBindings/` mirror Rust types from the StartOS core. When Rust types change, the corresponding TypeScript bindings need updating. These are re-exported through `base/lib/osBindings/index.ts`.

### Writing Tests

- Place test files next to the code they test, or in the `test/` directory
- Use the `.test.ts` extension
- Tests run in Node.js with ts-jest — no browser environment

## Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
feat(sdk): add WebSocket health check
fix(sdk): correct ExVer range parsing for pre-release versions
test(sdk): add coverage for MultiHost port binding
```

See the root [CONTRIBUTING.md](../CONTRIBUTING.md#commit-messages) for the full convention.
