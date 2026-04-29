# Structure Outline

## Approach

Build `ServicesPreferencesService` as the reactive state backbone, then wire it into the dashboard in two vertical slices (sort persistence, then filter UI). Each phase is independently testable and adds user-visible value.

---

## Phase 1: ServicesPreferencesService with Persistence

Create the service that manages sort column/direction and filter state (text query + lifecycle state toggles) as reactive `BehaviorSubject` streams, with full cross-session persistence via `StorageService`.

**Files**:
- `web/projects/ui/src/app/services/services-preferences.service.ts` (new)

**Key changes**:
- `ServicesPreferencesService` — new class
  - `sortState$: BehaviorSubject<{ column: 'name' | 'status'; direction: 'asc' | 'desc' }>`
  - `filterState$: BehaviorSubject<{ query: string; states: PrimaryStatus[] }>`
  - `init()`: reads `'_startos/services/preferences'` from `StorageService`, falls back to defaults
  - `setSort(column, direction)`: updates `sortState$`, persists
  - `setQuery(query)`: updates `filterState$`, persists
  - `toggleState(status)`: toggles a `PrimaryStatus` in `states`, persists
  - `resetFilter()`: clears query and states to default
- Key shape: `{ sort: { column, direction }, filter: { query, states } }`
- Follows `ClientStorageService` init-on-inject pattern: reads storage in constructor, emits via `ReplaySubject` for late subscribers

**Verify**: `ng test --no-watch --browsers=ChromeHeadless --include=**/services-preferences.service.spec.ts` — service injectable, emits correct defaults, persists and restores values. Manual: `localStorage.getItem('_startos/services/preferences')` shows expected JSON after calling setters.

---

## Phase 2: Sort Persistence End-to-End

Fix the `TableComponent` wrapper to expose `sortChange` output, then wire it into the services dashboard so sort state survives navigation and browser restart.

**Files**:
- `web/projects/ui/src/app/routes/portal/components/table.component.ts` (modify)
- `web/projects/ui/src/app/routes/portal/routes/services/dashboard/table.component.ts` (modify)
- `web/projects/ui/src/app/routes/portal/routes/services/dashboard/dashboard.component.ts` (modify)

**Key changes**:
- `TableComponent.hostDirectives`: add `outputs: ['sortChange']` to the `TuiTableDirective` mapping
  ```typescript
  hostDirectives: [
    {
      directive: TuiTableDirective,
      inputs: ['sorter'],
      outputs: ['sortChange'],
    },
  ]
  ```
- `ServicesTableComponent`:
  - Inject `ServicesPreferencesService`
  - Subscribe to `sortChange` output → call `prefs.setSort(column, direction)`
  - On init, call `prefs.getSortState()` to set initial `direction` signal and `sorter` comparator
- `ServicesDashboardComponent`: inject `ServicesPreferencesService`, pass `sortState$` to table component (or let table component inject directly)

**Verify**: `ng test` passes. Manual: click a column header to sort, navigate away from Services tab, navigate back — table is sorted by the previous column. Close browser, reopen — sort is restored.

---

## Phase 3: Filter UI End-to-End

Add text search input and lifecycle state toggle buttons to the dashboard, wire filtering logic, and persist filter state.

**Files**:
- `web/projects/ui/src/app/routes/portal/routes/services/dashboard/dashboard.component.ts` (modify)
- `web/projects/ui/src/app/routes/portal/routes/services/dashboard/dashboard.component.html` (modify)
- `web/projects/ui/src/app/pipes/services-filter.pipe.ts` (new) — optional, only if filtering logic warrants a pipe

**Key changes**:
- Dashboard template additions:
  - Text search `<input>` bound to `filterState$.query` via two-way binding or `(input)` handler
  - Lifecycle state toggle buttons (All, Running, Installing, Updating, Stopped, Error, etc.) — each toggles a `PrimaryStatus` in `filterState$.states`
  - Filtered services array: `services() | servicesFilter: (filterState$.query) : (filterState$.states)`
- `ServicesFilterPipe` (if created):
  - `transform(items: PackageDataEntry[], query: string, states: PrimaryStatus[]): PackageDataEntry[]`
  - Text search: Fuse.js fuzzy (same strategy as `FilterPackagesPipe` — short queries `threshold: 0.2`, long queries `useExtendedSearch: true`)
  - State filter: `items.filter(p => states.includes('all') || states.includes(renderPkgStatus(p).primary))`
  - Returns spread copies to avoid mutation
- If filtering is simple enough, skip the pipe and use a `computed` in the component instead

**Verify**: `ng test` passes. Manual: type in search box — table filters in real-time. Click lifecycle toggles — only matching services shown. Clear search + reset toggles — all services visible. Navigate away and back — filter state restored.

---

## Phase 4: Jest Setup + Tests

Install Jest, configure for Angular 21, write initial tests for the service and status rendering.

**Files**:
- `web/projects/ui/package.json` (modify — add devDependencies, test script)
- `web/projects/ui/jest.config.js` (new)
- `web/projects/ui/tsconfig.spec.json` (new or modify)
- `web/projects/ui/src/app/services/services-preferences.service.spec.ts` (new)
- `web/projects/ui/src/app/services/pkg-status-rendering.service.spec.ts` (new)

**Key changes**:
- DevDependencies: `jest`, `@types/jest`, `jest-preset-angular`, `ts-jest`
- `jest.config.js`: `ts-jest` preset, `jest-preset-angular` setup file, coverage config
- `services-preferences.service.spec.ts`:
  - Test: default values emit correctly
  - Test: `setSort()` updates stream and persists to `StorageService`
  - Test: `init()` restores from `StorageService`
  - Test: `toggleState()` toggles status in array
  - Test: `resetFilter()` clears to defaults
- `pkg-status-rendering.service.spec.ts`:
  - Test: `renderPkgStatus()` derives correct `PrimaryStatus` for each `stateInfo.state` variant
  - Test: `getInstalledPrimaryStatus()` maps `statusInfo` + `tasks` correctly

**Verify**: `ng test` — all new tests pass. `ng test --coverage` — coverage for `ServicesPreferencesService` > 90%.

---

## Testing Checkpoints

| After Phase | What should be true |
|---|---|
| Phase 1 | `ServicesPreferencesService` exists, injectable, emits correct defaults, persists/restores via `StorageService` |
| Phase 2 | Sort state persists across navigation and browser restart; `TableComponent` exposes `sortChange` output |
| Phase 3 | Text search and lifecycle toggles work end-to-end; filter state persists across navigation and browser restart |
| Phase 4 | Jest runs; `ServicesPreferencesService` tests pass; `renderPkgStatus()` tests pass; coverage > 80% |
