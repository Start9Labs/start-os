# Contributing

## Prerequisites

- **Rust** — stable toolchain (edition 2018+)
- **Node.js** — v16+ with npm

## Building

### Rust

```bash
cargo build                        # Build all crates
cargo build --features debug       # Build with tracing support
```

### TypeScript client

```bash
cd client
npm install
npm run build    # Compiles to dist/
npm run check    # Type-check without emitting
```

## Testing

### Rust

```bash
cargo test                 # Run all tests
cargo test -p patch-db     # Core crate only
cargo test -p json-ptr     # JSON Pointer crate only
cargo test -p json-patch   # JSON Patch crate only
```

The core crate uses `proptest` for property-based testing.

### TypeScript

The client uses pre-commit hooks (husky) for linting:

```bash
cd client
npx prettier --check "**/*.{js,ts,html,md,less,json}"
npx tslint --project .
```

## CLI utility

`patch-db-util` provides commands for inspecting and restoring database files:

```bash
# Dump database state as JSON
cargo run -p patch-db-util -- dump path/to/my.db

# Restore database from JSON on stdin
echo '{"count": 42}' | cargo run -p patch-db-util -- from-dump path/to/my.db
```

## Code style

### Rust

- Follow standard `rustfmt` conventions
- Use `thiserror` for error types
- Async functions use `tokio`

### TypeScript

- Prettier for formatting (runs via pre-commit hook)
- TSLint for linting (runs via pre-commit hook)
- RxJS conventions: suffix observables with `$`

## Making changes

1. **Check [ARCHITECTURE.md](ARCHITECTURE.md)** to understand which crate(s) your change touches
2. **Follow existing patterns** — look at neighboring code before inventing new abstractions
3. **Cross-layer changes** (Rust types that affect the TS client) require updating both sides to keep the wire format compatible:
   - `Revision` and `Dump` types must match between `core/src/patch.rs` and `client/lib/types.ts`
   - Patch operations (add/remove/replace) must match between `json-patch/` and `client/lib/json-patch-lib.ts`
4. **Run tests** before submitting

## Commit conventions

- Use imperative mood in commit messages ("add feature", not "added feature")
- Keep commits focused — one logical change per commit
