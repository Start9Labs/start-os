# CLAUDE.md

patch-db is a JSON Patch–based database with a Rust backend and TypeScript client.

## Where to look

| Need | File |
|------|------|
| What this project is, quick start examples | [README.md](README.md) |
| Project structure, crate details, data flow, storage format, concurrency | [ARCHITECTURE.md](ARCHITECTURE.md) |
| Build commands, testing, code style, making changes | [CONTRIBUTING.md](CONTRIBUTING.md) |

## Key files

| Area | Path |
|------|------|
| Core API | `core/src/store.rs` — `PatchDb`, `TypedPatchDb`, `Store`, `MutateResult` |
| Types | `core/src/patch.rs` — `Revision`, `Dump`, `DiffPatch`, `diff()` |
| Subscriptions | `core/src/subscriber.rs` — `DbWatch`, `TypedDbWatch`, `Subscriber`, `Broadcast` |
| Model system | `core/src/model.rs` — `HasModel`, `Model`, `ModelExt`, `Pointer` |
| Derive macro | `macro-internals/src/lib.rs` — `HasModel` derive implementation |
| Error types | `core/src/lib.rs` — `Error` enum, re-exports |
| JSON Pointer | `json-ptr/src/lib.rs` — `JsonPointer`, `ROOT`, path navigation |
| JSON Patch | `json-patch/src/lib.rs` — `Patch`, `PatchOperation`, `diff()`, `patch()` |
| TS client | `client/lib/patch-db.ts` — `PatchDB<T>` class |
| TS patch lib | `client/lib/json-patch-lib.ts` — client-side patch application |
| TS types | `client/lib/types.ts` — `Revision`, `Dump`, `Update`, `PatchOp` |

## Operating rules

- **Wire format** — Rust and TS define `Revision`, `Dump`, and patch operations independently. Changes to one side must be mirrored in the other. See the cross-layer section in [CONTRIBUTING.md](CONTRIBUTING.md#making-changes).
- **Patch operations** — Only `add`, `remove`, and `replace` are used. The TS client does not implement `test`, `move`, or `copy`.
- **Immutable patch application** — The TS client applies patches by shallow-copying objects/arrays, not mutating in place. This is intentional for UI framework change detection.
- **`HasModel` derive** — Respects serde attributes (`rename_all`, `rename`, `flatten`, `tag`, `content`). Generated accessors follow the pattern `as_<field>()`, `as_<field>_mut()`, `into_<field>()`.
- **Error handling** — Rust uses `thiserror` with the `Error` enum in `core/src/lib.rs`. TS does not have formal error types.
