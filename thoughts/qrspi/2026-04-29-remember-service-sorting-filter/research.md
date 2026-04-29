# Research Findings

## Q1: How does Taiga UI's `TuiTable` / `tuiTableSort` pipe handle sorting state, and is there a built-in mechanism to persist or control the active sort column and direction?

### Findings

- The services dashboard table is defined in `web/projects/ui/src/app/routes/portal/routes/services/dashboard/table.component.ts:18-46`.
- It uses the `tuiTableSort` pipe in the template at line 28: `@for (service of services() | tuiTableSort; track $index)`.
- The base `TableComponent` (`web/projects/ui/src/app/routes/portal/components/table.component.ts:1-44`) wraps Taiga's `TuiTableDirective` via `hostDirectives` at line 33-36:
  ```typescript
  hostDirectives: [
    {
      directive: TuiTableDirective,
      inputs: ['sorter'],
    },
  ]
  ```
- The directive exposes a `sorter` input (`TuiComparator<any> | null`) at `table.component.ts:36`. Individual column headers use `[sorter]="appTableSorters()[$index] || null"` at line 18.
- The services dashboard table passes sorters via `[appTableSorters]="[null, name, status]"` at `table.component.ts:31` — only the "Name" and "Status" columns have comparators.
- The `ServicesTableComponent` defines two comparators:
  - `name` comparator at line 62: alphabetical by manifest title (case-insensitive).
  - `status` comparator at lines 58-60: uses `getInstalledPrimaryStatus()` from `pkg-status-rendering.service.ts:58-70` to compare installed primary status.
- **No built-in persistence mechanism exists.** The `TuiTableDirective` and `tuiTableSort` pipe are stateless — sorting is applied per-render via the pipe. There is no `sortState` input, no `@Output` for sort changes, and no mechanism to read the current sort direction. The `sorter` input only provides the comparator function; it does not expose or control active sort state.
- No code in the codebase reads or writes sort state to localStorage or any other persistence layer.

## Q2: What patterns exist in the codebase for persisting user preferences to `localStorage` — specifically how `StorageService` and `ClientStorageService` are used, and what key naming conventions are followed?

### Findings

- `StorageService` (`web/projects/ui/src/app/services/storage.service.ts:1-30`) is a thin wrapper around `WA_LOCAL_STORAGE` (`@ng-web-apis/common`).
  - Key prefix: `'_startos/'` (line 5).
  - `get<T>(key)` at line 13: reads, parses JSON, returns typed value.
  - `set(key, value)` at line 17: stringifies and writes.
  - `clear()` at line 21: clears all storage.
  - `migrate036()` at line 23: migrates old `_embassystorage/_embassykv/` keys to the new prefix.
- `ClientStorageService` (`web/projects/ui/src/app/services/client-storage.service.ts:1-24`) uses `StorageService` for reactive key-value persistence.
  - Key: `'SHOW_DEV_TOOLS'` (line 5) — a single uppercase constant.
  - Exposes `showDevTools$` as a `ReplaySubject<boolean>(1)` for reactive consumption.
  - `init()` reads the value on startup; `toggleShowDevTools()` writes and emits.
- **Key naming convention:** Simple uppercase strings (e.g., `'SHOW_DEV_TOOLS'`). No hierarchical or namespaced keys beyond the `_startos/` prefix in `StorageService`.
- **No pattern for persisting sort state** exists anywhere in the codebase. The only persisted preferences are `loggedIn`, `patchDB`, and `SHOW_DEV_TOOLS`.

## Q3: How does the `FilterPackagesPipe` in the marketplace project work, and how does `AbstractCategoryService` manage filter state (query string, category) as reactive streams?

### Findings

- `FilterPackagesPipe` (`web/projects/marketplace/src/pipes/filter-packages.pipe.ts:1-60`) transforms `MarketplacePkg[]` given a `query` and `category`.
  - **Query filtering** (lines 12-43): Uses Fuse.js for fuzzy search.
    - Short queries (< 4 chars): `threshold: 0.2`, `location: 0`, `distance: 16`, searches `title` (weight 1) and `id` (weight 0.5).
    - Long queries (>= 4 chars): `ignoreLocation: true`, `useExtendedSearch: true`, searches `title`, `id`, `description.short` (weight 0.4), `description.long` (weight 0.1). Query is prefixed with `'` for extended search syntax.
  - **Category filtering** (lines 47-55): Filters by `p.categories.includes(category)`, defaults to `'all'` for no filter. Sorts by `publishedAt` descending.
  - Returns spread copies (`{ ...a }`) to avoid mutation.
- `AbstractCategoryService` (`web/projects/marketplace/src/services/category.service.ts:1-18`) is an abstract base class with two `BehaviorSubject` streams:
  - `category$` initialized to `'all'` (line 5).
  - `query$` initialized to `''` (line 6).
  - Abstract methods: `getCategory$()`, `changeCategory()`, `setQuery()`, `getQuery$()`, `resetQuery()`, `handleNavigation()`.
- `CategoryService` (`web/projects/ui/src/app/services/category.service.ts:1-28`) extends `AbstractCategoryService`, implementing all abstract methods by delegating to the `BehaviorSubject` instances. `handleNavigation()` calls `router.navigate([])`.
- **Connection flow:**
  1. `MarketplaceComponent` (`marketplace.component.ts:47-50`) injects `AbstractCategoryService` and exposes `category$` and `query$` as observables.
  2. `MenuComponent` (`menu.component.ts:33-58`) subscribes to both streams, keeping local `category` and `query` signal state in sync.
  3. `MenuComponent` emits changes via `onCategoryChange()` (line 52) and `onQueryChange()` (line 57), which call `categoryService.changeCategory()` and `categoryService.setQuery()`.
  4. The template at `marketplace.component.ts:38-40` pipes packages through `filterPackages: (query$ | async) : (category$ | async)`.
  5. `CategoriesComponent` (`categories.component.ts:37-43`) emits category changes via `(categoryChange)` output.
  6. `SearchComponent` (`search.component.ts:1-17`) emits query changes via `(queryChange)` output using `ngModel` two-way binding.
- **No persistence** of category or query state exists in the marketplace. State lives only in the `BehaviorSubject` instances while the component tree is mounted.

## Q4: What is the full shape of `PackageDataEntry` / `StateInfo` in `DataModel`, and what fields are available for filtering (e.g., service state/status, version, health)?

### Findings

- `DataModel` (`web/projects/ui/src/app/services/patch-db/data-model.ts:6-10`):
  ```typescript
  type DataModel = {
    ui: UIData
    serverInfo: T.ServerInfo & { language: Languages; keyboard: FullKeyboard | null }
    packageData: AllPackageData
  }
  ```
- `UIData` (lines 11-16): `name`, `registries`, `snakeHighScore`, `startosRegistry`.
- `AllPackageData` (line 21): `NonNullable<T.AllPackageData & Record<string, PackageDataEntry<StateInfo>>>`.
- `PackageDataEntry<T extends StateInfo = StateInfo>` (lines 18-20): Extends `T.PackageDataEntry` (from `@start9labs/start-sdk`) with `stateInfo: T`.
- `StateInfo` (line 23): Union of `InstalledState | InstallingState | UpdatingState`.
- `InstalledState` (lines 25-28): `{ state: 'installed' | 'removing'; manifest: T.Manifest; installingInfo?: undefined }`.
- `InstallingState` (lines 30-33): `{ state: 'installing' | 'restoring'; installingInfo: InstallingInfo; manifest?: undefined }`.
- `UpdatingState` (lines 35-38): `{ state: 'updating'; installingInfo: InstallingInfo; manifest: T.Manifest }`.
- `InstallingInfo` (lines 40-43): `{ progress: T.FullProgress; newManifest: T.Manifest }`.
- **Fields available for filtering:**
  - `stateInfo.state` — the lifecycle state (`'installed' | 'removing' | 'installing' | 'restoring' | 'updating'`).
  - `stateInfo.manifest` (when present) — contains `id`, `version`, and other manifest fields from `T.Manifest`.
  - `stateInfo.installingInfo.progress` — progress of install/update operations.
  - `statusInfo` (inherited from `T.PackageDataEntry`) — contains `desired.main`, `started`, `health`, `error`.
  - `tasks` (inherited from `T.PackageDataEntry`) — task info used for `'task-required'` status.
  - `getInstalledPrimaryStatus()` (`pkg-status-rendering.service.ts:58-70`) derives a `PrimaryStatus` from `statusInfo` and `tasks`.
  - `renderPkgStatus()` (`pkg-status-rendering.service.ts:37-52`) produces `{ primary: PrimaryStatus; health: T.HealthStatus | null }`.
  - `PrimaryStatus` type (`pkg-status-rendering.service.ts:102`): `'installing' | 'updating' | 'removing' | 'restoring' | 'starting' | 'running' | 'stopping' | 'restarting' | 'stopped' | 'backing-up' | 'error' | 'task-required'`.
  - `BaseStatus` (`pkg-status-rendering.service.ts:97`): same minus `'task-required'`.

## Q5: How does the `PatchDB` service provide reactive data streams, and what is the lifecycle of the `watch$('packageData')` observable used in the dashboard?

### Findings

- `PatchDbSource` (`web/projects/ui/src/app/services/patch-db/patch-db-source.ts:1-58`) is an `Observable<Update<DataModel>[]>` provided in root.
- **Stream construction** (lines 20-40):
  1. Starts from `AuthService.isVerified$` — waits for authentication.
  2. Calls `api.subscribeToPatchDB({})` to get initial `dump` and `guid`.
  3. Opens a websocket via `api.openWebsocket$<Revision>(guid)`.
  4. Buffers revisions in 250ms windows (`bufferTime(250)` at line 30).
  5. Filters out empty buffers (`filter(revisions => !!revisions.length)` at line 31).
  6. Preloads the initial dump with `startWith([dump])` at line 32.
  7. Error recovery: on error, triggers `state.retrigger(false, 2000)`, waits for `'running'` state, then resubscribes to `original$` (`skip(1)`, `take(1)` at lines 36-39).
  8. Initial value from `LocalStorageBootstrap.init()` via `startWith` at line 42.
- **Dashboard usage** (`dashboard.component.ts:44-52`):
  ```typescript
  readonly services = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('packageData')
      .pipe(
        map(pkgs => Object.values(pkgs)),
        shareReplay(1),
      ),
    { initialValue: null },
  )
  ```
  - Injects `PatchDB<DataModel>` (the PatchDB token).
  - Calls `watch$('packageData')` to get the full package data map.
  - Maps to array of values, shares with `shareReplay(1)`.
  - Converts to a signal via `toSignal` with `initialValue: null`.
- **Lifecycle:** The observable is created on component construction and never explicitly unsubscribed. `toSignal` handles cleanup when the component is destroyed (Angular 21 signal integration). The `shareReplay(1)` ensures multiple consumers get the latest value without re-subscribing.
- **`PATCH_CACHE` injection token** (`patch-db-source.ts:17-21`): Provides a `BehaviorSubject<Dump<DataModel>>` initialized with an empty dump. Used for caching.
- **`watch$('packageData', id)`** (single package): Used in `MarketplaceControlsComponent` (`controls.component.ts:72-74`) to watch a specific package by ID.

## Cross-Cutting Observations

1. **No sort state persistence anywhere.** The services dashboard table uses `tuiTableSort` purely for display-time sorting with no mechanism to remember or restore the user's sort choice.
2. **StorageService is the sole localStorage abstraction.** All persisted keys go through `StorageService` with the `_startos/` prefix. `ClientStorageService` is a thin reactive wrapper for a single key.
3. **Marketplace filtering is fully reactive but ephemeral.** `AbstractCategoryService` uses `BehaviorSubject` for category and query state, but nothing persists these to storage.
4. **Status rendering is centralized.** `pkg-status-rendering.service.ts` provides `getInstalledPrimaryStatus()` and `renderPkgStatus()` — the single source of truth for status derivation from `PackageDataEntry`.
5. **The `stateInfo.state` discriminator** is the key field for filtering packages by lifecycle stage. It's a string union (`'installed' | 'removing' | 'installing' | 'restoring' | 'updating'`) that can be checked with type guards in `get-package-data.ts`.

## Open Areas

- The `TuiTableDirective` API from Taiga UI is not fully inspectable from the codebase alone — specifically whether it exposes a `sortState` output or input that could be leveraged for persistence. The codebase uses it only for the `sorter` input.
- `T.PackageDataEntry` and `T.Manifest` from `@start9labs/start-sdk` are external types; their full shape (especially `statusInfo` and `tasks` fields) is not directly visible in this codebase.
- `LocalStorageBootstrap` (`web/projects/ui/src/app/services/patch-db/local-storage-bootstrap.ts`) is referenced but not read — its role in cache initialization is unclear from this scope.
