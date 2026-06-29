# patch-db Code Audit

## Critical / High Severity

### Rust

#### 1. Infinite loop in `run_idempotent` — FIXED

**File:** `patch-db/src/store.rs`

`old` was read once before the loop, never refreshed. If another writer modified `store.persistent` between the initial read and the write-lock acquisition, the `&old == &store.persistent` check failed forever — `old` was never updated, so the loop spun infinitely.

**Fix:** Moved `old` read inside the loop so it refreshes on each retry attempt.

#### 2. `TentativeUpdated` undo after successful disk write — FIXED

**File:** `patch-db/src/store.rs`

If `compress()` succeeded (wrote patched state to disk) but a later step failed, `Drop` rolled back the in-memory state while the on-disk state already reflected the patch. On next startup, the file replayed the patch, creating a permanent divergence.

**Fix:** Rewrote `compress()` with three explicit phases (atomic backup, main file rewrite, non-fatal backup removal). Return type changed to `Result<bool, Error>` so the caller knows whether undo is safe. If the backup was committed, `TentativeUpdated` disarms the undo.

#### 3. `push_start_idx` doesn't update existing segment ranges — FIXED

**File:** `json-ptr/src/lib.rs`

Unlike `push_start` which shifted all existing segment ranges by the prefix length, `push_start_idx` just inserted a new segment without adjusting the others. All existing segments' ranges became wrong, causing corrupted pointer lookups or panics.

**Fix:** Added the range-shifting loop to match `push_start` behavior.

#### 4. Integer underflow in `DiffPatch::rebase` for Remove — FIXED

**File:** `patch-db/src/patch.rs`

When `idx == 0` and `onto_idx == 0`, the condition `idx >= onto_idx` passed and `idx - 1` underflowed a `usize`. Panics in debug, wraps to `usize::MAX` in release.

**Fix:** Changed condition from `idx >= onto_idx` to `idx > onto_idx`.

### TypeScript

#### 5. Remove on nested array elements corrupts state — FIXED

**File:** `client/lib/json-patch-lib.ts`

`recursiveApply` returned `undefined` for remove (since `value` is undefined on `RemoveOperation`). For arrays, the splice-based removal only kicked in when `path.length === 1`. A deeper path like `/arr/0/nested/2` set `array[2] = undefined` instead of splicing it out, leaving a hole.

**Fix:** Introduced a `REMOVE_SENTINEL` Symbol. Base case returns the sentinel for remove ops. Array and object handlers check for it to trigger proper splice/delete.

#### 6. RFC 6902 `"-"` (end-of-array) token not handled — FIXED

**File:** `client/lib/json-patch-lib.ts`

`parseInt("-")` returned `NaN`. `splice(NaN, 0, value)` inserted at position 0 instead of appending. Non-numeric path segments on arrays also silently produced corrupt state.

**Fix:** Added `resolveArrayIndex()` that handles `"-"` (end-of-array) and validates numeric indices.

#### 7. Revision gap silently applied — FIXED

**File:** `client/lib/patch-db.ts`

The check `update.id < expected` only deduplicated. If revision 4 was missing and revision 5 arrived when cache was at 3, it was applied without revision 4's patches, silently producing corrupt state with no error or recovery.

**Fix:** Added `console.warn` when a revision gap is detected.

---

## Medium Severity

### Rust

#### 8. `unreachable!()` reachable via deserialized patches — IGNORED

**File:** `patch-db/src/patch.rs`

`DiffPatch` is `Deserialize`-able and wraps `Patch` which can hold Move/Copy/Test operations. `for_path`, `rebase`, `exists`, and `keys` all panic on those variants.

**Status:** Ignored. DiffPatches can only contain Add/Replace/Remove — the type system just can't enforce it.

#### 9. `poll_changed` applies only one revision per call — FIXED

**File:** `patch-db/src/subscriber.rs`

If multiple revisions queued up, the `Stream` implementation applied one at a time, emitting intermediate states that may never have been a consistent committed state.

**Fix:** Added a drain loop after the first `poll_recv` wake to consume all queued revisions before returning.

#### 10. `compress` backup removal failure clobbers good data — FIXED

**File:** `patch-db/src/store.rs`

If `remove_file(bak)` failed after the main file was successfully rewritten, the error propagated. On next open, `Store::open` saw the stale `.bak` file and renamed it over the successfully compacted main file.

**Fix:** Backup removal is now non-fatal (`let _ = ...`). A leftover backup is harmlessly replayed on restart since it matches the main file content.

#### 11. `DbWatch::sync` permanently desynchronizes on patch error — IGNORED

**File:** `patch-db/src/subscriber.rs`

If one patch fails to apply, the error returns immediately. Remaining queued revisions are never consumed. The watch is now permanently out of sync with no recovery path.

**Status:** Ignored. Patch errors should be impossible; if they occur, a different bug in patch-db is the root cause. Not worth the complexity to drain-and-continue.

#### 12. RFC 6902 deviation: `replace` on missing path silently adds — IGNORED (intentional)

**File:** `json-patch/src/lib.rs`

RFC 6902 §4.3 requires the target location to exist. The implementation falls back to `add` instead of returning an error.

**Status:** Ignored. This is intentional behavior for this project's use case.

### TypeScript

#### 13. `NonNullable` return types on `watch$` are unsound — FIXED

**File:** `client/lib/patch-db.ts`

All `watch$` overloads claimed `NonNullable<...>`, but runtime values can be `null`/`undefined` (e.g., after a `remove` operation). Consumers skipped null checks based on the type, leading to runtime crashes.

**Fix:** Removed outer `NonNullable` wrapper from the 1-level overload return type.

#### 14. `withLatestFrom` + in-place mutation fragility — FIXED

**File:** `client/lib/patch-db.ts`

`processUpdates` mutated the cache object in place, then re-emitted it via `cache$.next(cache)`. If `source$` emitted synchronously twice before the subscriber ran, `withLatestFrom` sampled the already-mutated object reference, potentially skipping valid revisions via the stale `cache.id` check.

**Fix:** Replaced `withLatestFrom` with direct `this.cache$.value` access in the subscribe callback.

---

## Low Severity / Performance

### Rust

#### 15. `OPEN_STORES` never removes entries — FIXED

**File:** `patch-db/src/store.rs`

Entries were inserted on open but never cleaned up on close. Unbounded growth over the lifetime of a process that opens many different database files.

**Fix:** `Store::close()` now removes the entry from `OPEN_STORES`.

#### 16. `Broadcast::send` clones patches per subscriber under write lock — DEFERRED

**File:** `patch-db/src/subscriber.rs`

`revision.for_path()` is called per subscriber, cloning and filtering the entire patch. This is O(subscribers × operations) work while holding the `RwLock` write guard.

#### 17. Array diff is O(n²) — DEFERRED

**File:** `json-patch/src/diff.rs`

The `ptr_eq` scan inside the array diff loops is O(n) per element, making worst-case O(n²) for large arrays with mostly non-pointer-equal elements.

#### 18. `Store::exists` conflates null with missing — FIXED

**File:** `patch-db/src/store.rs`

A key with an explicit `Value::Null` was reported as non-existent.

**Fix:** Changed from null comparison to `.is_some()`.

### TypeScript

#### 19. `new RegExp` in hot path — FIXED

**File:** `client/lib/json-patch-lib.ts`

`arrayFromPath` and `pathFromArray` constructed new `RegExp` objects on every call.

**Fix:** Pre-compiled regex literals at module scope.

#### 20. O(watchedNodes × patchOps) redundant `arrayFromPath` — FIXED

**File:** `client/lib/patch-db.ts`

Inside `handleRevision`, `arrayFromPath(path)` was called for every `(watchedNode, patchOp)` pair.

**Fix:** Pre-convert patch operation paths once outside the loop.

#### 21. `getValueByPointer` swallows all exceptions — FIXED

**File:** `client/lib/json-patch-lib.ts`

The `catch (e) { return undefined }` masked programming errors and state corruption, making debugging very difficult.

**Fix:** Now only catches `TypeError` (from accessing properties on null/undefined), re-throws everything else.

#### 22. `throw 'unreachable'` is reachable — FIXED

**File:** `client/lib/json-patch-lib.ts`

If a patch path extended past the actual document depth (data is a primitive at a non-terminal segment), this branch executed. It threw a string with no stack trace.

**Fix:** Now throws a proper `Error` with a descriptive message.
