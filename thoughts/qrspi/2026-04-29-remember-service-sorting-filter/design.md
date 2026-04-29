# Design Discussion

## Current State

The Services dashboard (`web/projects/ui/src/app/routes/portal/routes/services/dashboard/`) displays installed services in a table with Taiga UI's `tuiTableSort` pipe (`table.component.ts:28`). Sorting is applied purely at render time — the pipe is stateless and there is no `sortState` input or output on `TuiTableDirective`. The only `sorter` input (`table.component.ts:36`) provides comparator functions but no state tracking.

Two comparators exist in `ServicesTableComponent`: `name` (line 62, alphabetical by manifest title) and `status` (lines 58-60, using `getInstalledPrimaryStatus()` from `pkg-status-rendering.service.ts:58-70`).

Filtering does not exist at all on the services table. The marketplace has `FilterPackagesPipe` (`filter-packages.pipe.ts:1-60`) with Fuse.js fuzzy search and category filtering, but it is not used in the services dashboard.

No user preferences are persisted for the services table anywhere in the codebase. The only persisted keys are `loggedIn`, `patchDB` (via `StorageService` at `storage.service.ts:1-30`), and `SHOW_DEV_TOOLS` (via `ClientStorageService` at `client-storage.service.ts:1-24`).

## Desired End State

A `ServicesPreferencesService` manages sort column/direction and filter state (text query + lifecycle state toggles) as reactive `BehaviorSubject` streams. On init, it restores values from `StorageService` under the key `'_startos/services/preferences'`. On every change, it writes back to storage.

The dashboard component subscribes to these streams and applies them to the services array:
- **Sort**: After the `tuiTableSort` pipe renders, a secondary sort step applies the persisted sort column and direction.
- **Filter**: A `ServicesFilterPipe` (or inline pipe chain) applies text search + lifecycle state filtering before the sort pipe.

The table header sort indicators (click-to-toggle) remain functional via Taiga's built-in UI. The user's sort choice is restored on navigation back to the services tab.

**Verification**: Navigate away from Services tab, change sort/filter, return — table reflects the previous state. Close and reopen browser — state is restored.

## Patterns to Follow

**Good patterns to mirror:**

1. **`AbstractCategoryService` / `CategoryService` pattern** (`category.service.ts:1-28`, `category.service.ts:1-18`): Use `BehaviorSubject<boolean | string>` for reactive state, expose as `Observable`, persist on change. This is the established pattern for user preferences in the codebase.

2. **`StorageService` key hierarchy** (`storage.service.ts:5`): Use `'_startos/'` prefix. Store preferences as a single JSON object under `'_startos/services/preferences'` with shape `{ sort: { column: string; direction: 'asc' | 'desc' }, filter: { query: string; states: string[] } }`.

3. **`ClientStorageService` reactive wrapper** (`client-storage.service.ts:1-24`): Use `ReplaySubject<T>(1)` for values that need to emit their current value to late subscribers. Initialize from storage in `init()`.

4. **`toSignal` + `shareReplay` pattern** (`dashboard.component.ts:44-52`): Use `toSignal` for converting observables to signals in components. Use `shareReplay(1)` for shared reactive data.

5. **`Fuse.js` fuzzy search** (`filter-packages.pipe.ts:12-43`): Reuse the same fuzzy search strategy — short queries (< 4 chars) with `threshold: 0.2`, long queries with `useExtendedSearch: true`.

**Patterns to NOT follow:**

- The marketplace's `FilterPackagesPipe` sorts by `publishedAt` descending (`filter-packages.pipe.ts:53`) — not relevant here; we sort by user choice.
- The `AbstractCategoryService` has no persistence layer — our service must add it.
- Do not create a separate pipe for filtering if the filtering logic is simple enough to be in the component's `computed` — only create a pipe if it will be reused elsewhere.

## Design Decisions

1. **Sort state tracking**: Keep `tuiTableSort` for UI (click-to-toggle, direction indicators). Taiga's `TuiTableDirective` exposes `sortChange: OutputEmitterRef<TuiTableSortChange<T>>` (from `table.directive.d.ts:15`) which emits `{ sortComparator, sortDirection }` on every header click, and `direction: ModelSignal<TuiSortDirection>` for two-way binding. Subscribe to `sortChange` on the directive, persist the result to `ServicesPreferencesService`, and restore on init by setting the `direction` and `sorter` ModelSignals. This is cleaner than re-sorting the array after the pipe renders — the UI and state stay in sync naturally.

   **Note**: Our `TableComponent` wrapper (`table.component.ts:33-36`) only exposes `sorter` as an input. We must add `sortChange` output to its `hostDirectives` config or use `TuiTableDirective` directly in the services table component.

2. **Filter scope**: Lifecycle state toggles (All, Running, Installing, Updating, etc.) + text search. Lifecycle states derive from `PrimaryStatus` (`pkg-status-rendering.service.ts:102`): `'installing' | 'updating' | 'removing' | 'restoring' | 'starting' | 'running' | 'stopping' | 'restarting' | 'stopped' | 'backing-up' | 'error' | 'task-required'`. Filter by `PrimaryStatus` (via `renderPkgStatus()` at `pkg-status-rendering.service.ts:37-52`), **not** raw `stateInfo.state`. Confirmed: `stateInfo.state` has 5 raw values (`'installed' | 'removing' | 'installing' | 'restoring' | 'updating'`), while `PrimaryStatus` has 12 derived values. Two packages with `stateInfo.state === 'installed'` can have different visible states (`'running'` vs `'stopped'`), so `PrimaryStatus` is the correct discriminator.

3. **Service architecture**: `ServicesPreferencesService` extends `AbstractCategoryService` pattern (not the class directly, since it's abstract). Exposes `sortState$` and `filterState$` as `BehaviorSubject` streams. Auto-persists on every change via `StorageService`. Follows the `ClientStorageService` init-on-inject pattern.

4. **Key structure**: Single JSON object at `'_startos/services/preferences'`:
   ```json
   {
     "sort": { "column": "name" | "status", "direction": "asc" | "desc" },
     "filter": { "query": string, "states": string[] }
   }
   ```
   This avoids key sprawl and makes it easy to add more preferences later (e.g., column visibility).

5. **Persistence scope**: Full cross-session persistence via `localStorage` (through `StorageService`). Since we're already using `StorageService`, there's no reason to limit to session-only.

## Testing Framework

The codebase has no test framework configured (no Jest, Vitest, Karma, or Jasmine in `devDependencies`). This task includes adding **Jest** as the testing framework:

- Install `jest`, `@types/jest`, `jest-preset-angular`, and `ts-jest` as devDependencies.
- Configure `jest.config.js` in `web/projects/ui/` with `ts-jest` preset for TypeScript support.
- Configure `jest-preset-angular` for Angular component testing (template compilation, change detection).
- Add a `test` script to `web/projects/ui/package.json`.
- Write initial tests for `ServicesPreferencesService` (persistence, reactive streams) and `renderPkgStatus()` (PrimaryStatus derivation).

## What We're NOT Doing

- **Column visibility toggles**: Not adding show/hide column controls.
- **Per-service state management**: Not handling install/stop/start actions — that's the existing table row action system.
- **Filter presets**: Not adding saved filter profiles or "quick filters."
- **URL-based state**: Not syncing sort/filter to the URL (would enable sharing but adds complexity).
- **Marketplace filter reuse**: Not refactoring `FilterPackagesPipe` or `AbstractCategoryService` — we create a parallel service for services dashboard only.
- **`TuiTableDirective` source inspection**: Not digging into Taiga's source to find undocumented `sortState` outputs.

## Open Risks

1. **`TableComponent` wrapper blocks `sortChange` output** (`table.component.ts:33-36`): Our `TableComponent` only exposes `sorter` as an input in `hostDirectives` — it does NOT expose `sortChange` as an output. We must either add `sortChange` to the `hostDirectives` output mapping, or use `TuiTableDirective` directly in the services table component. This is a minor refactor but affects the sort persistence approach.

2. **Filter performance**: The services table could have dozens of entries. Fuse.js fuzzy search on every change (debounced) should be fine, but if the array is large, we may need to throttle or debounce the text search.

3. **Migration**: If we add this feature and later need to change the preferences key shape, we'll need a migration path similar to `StorageService.migrate036()` (`storage.service.ts:23`).

4. **Jest + Angular 21 compatibility**: Angular 21 uses the new control flow (`@if`, `@for`) and signals. `jest-preset-angular` may need configuration for the new template syntax. `ts-jest` with the right `tsconfig` target is critical. This is a greenfield test setup — there may be friction getting the first test to pass.
