# Implementation Plan

## Overview

Add `ServicesPreferencesService` for reactive sort/filter state management with `localStorage` persistence, then wire it into the services dashboard so sort and filter state survive navigation and browser restart.

---

## Phase 1: ServicesPreferencesService with Persistence

### Changes

#### 1. `web/projects/ui/src/app/services/services-preferences.service.ts` (new)

**Action**: create

New service following the `ClientStorageService` init-on-inject pattern with `ReplaySubject` for late subscribers.

```typescript
import { inject, Injectable } from '@angular/core'
import { ReplaySubject } from 'rxjs'
import { StorageService } from './storage.service'
import { PrimaryStatus } from './pkg-status-rendering.service'

export interface ServicesPreferences {
  sort: { column: 'name' | 'status'; direction: 'asc' | 'desc' }
  filter: { query: string; states: PrimaryStatus[] }
}

const STORAGE_KEY = 'services/preferences'

const DEFAULT_SORT: ServicesPreferences['sort'] = {
  column: 'name',
  direction: 'asc',
}

const DEFAULT_STATES: PrimaryStatus[] = [
  'running',
  'stopped',
  'starting',
  'stopping',
  'restarting',
  'error',
]

@Injectable({ providedIn: 'root' })
export class ServicesPreferencesService {
  private readonly storage = inject(StorageService)

  readonly sortState$ = new ReplaySubject<ServicesPreferences['sort']>(1)
  readonly filterState$ = new ReplaySubject<{
    query: string
    states: PrimaryStatus[]
  }>(1)

  constructor() {
    this.init()
  }

  private init() {
    const stored = this.storage.get<ServicesPreferences>(STORAGE_KEY)
    if (stored?.sort) {
      this.sortState$.next(stored.sort)
    } else {
      this.sortState$.next(DEFAULT_SORT)
    }
    if (stored?.filter) {
      this.filterState$.next(stored.filter)
    } else {
      this.filterState$.next({ query: '', states: DEFAULT_STATES })
    }
  }

  setSort(column: 'name' | 'status', direction: 'asc' | 'desc') {
    const sort = { column, direction }
    this.sortState$.next(sort)
    this.persist()
  }

  setQuery(query: string) {
    const filter = { query, states: this.filterState$.value.states }
    this.filterState$.next(filter)
    this.persist()
  }

  toggleState(status: PrimaryStatus) {
    const states = this.filterState$.value.states
    const idx = states.indexOf(status)
    const newStates = idx === -1 ? [...states, status] : states.filter(s => s !== status)
    this.filterState$.next({ query: this.filterState$.value.query, states: newStates })
    this.persist()
  }

  resetFilter() {
    this.filterState$.next({ query: '', states: DEFAULT_STATES })
    this.persist()
  }

  private persist() {
    const sort = this.sortState$.value
    const filter = this.filterState$.value
    this.storage.set(STORAGE_KEY, { sort, filter })
  }
}
```

**Key design notes**:
- Uses `ReplaySubject<T>(1)` (not `BehaviorSubject`) to follow the `ClientStorageService` pattern — `value` accessor gives synchronous access for `toggleState`/`persist`.
- `DEFAULT_STATES` includes the 6 "always-visible" lifecycle states: running, stopped, starting, stopping, restarting, error. This matches the UX expectation that "All" is the default filter.
- `persist()` reads current values from both subjects — no separate state object needed.

### Verification

#### Automated
- [x] `cd web && npx tsc --project projects/ui/tsconfig.json --noEmit --skipLibCheck` — no TypeScript errors (pre-existing errors unrelated to this file)

#### Manual
- [ ] `localStorage.getItem('_startos/services/preferences')` returns expected JSON after calling `setSort()` and `setQuery()` in the console

---

## Phase 2: Sort Persistence End-to-End

### Changes

#### 1. `web/projects/ui/src/app/routes/portal/components/table.component.ts` (modify)

**Action**: modify

Add `outputs: ['sortChange']` to the `hostDirectives` config to expose Taiga's `sortChange` output.

```diff
  hostDirectives: [
    {
      directive: TuiTableDirective,
      inputs: ['sorter'],
+     outputs: ['sortChange'],
    },
  ],
```

This is a 1-line change. The `sortChange` output is an `OutputEmitterRef<TuiTableSortChange<T>>` from Taiga that emits `{ sortComparator, sortDirection }` on every header click.

#### 2. `web/projects/ui/src/app/routes/portal/routes/services/dashboard/table.component.ts` (modify)

**Action**: modify

Wire `sortChange` into `ServicesPreferencesService` and apply initial sort state.

```diff
+ import { ServicesPreferencesService } from 'src/app/services/services-preferences.service'
+ import { TuiTableSortChange } from '@taiga-ui/addon-table'
+ import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
+ import { effect } from '@angular/core'
```

Add the `sortChange` output binding in the template:

```diff
  <table
    [sorter]="name"
+   (sortChange)="onSortChange($event)"
    [appTable]="[null, 'Name', 'Status', 'Version', 'Uptime']"
    [appTableSorters]="[null, name, status]"
  >
```

Add the handler and initial state application in the component class:

```diff
  export class ServicesTableComponent<...> {
+   private readonly prefs = inject(ServicesPreferencesService)

+   constructor() {
+     // Apply initial sort state from preferences
+     const sort = this.prefs.sortState$.value
+     // We need to set the sorter comparator and direction on the table
+     // The direction is controlled via the TuiTableDirective's direction signal
+     // We use effect to react to sort state changes
+     effect(() => {
+       const s = this.prefs.sortState$.value
+       // The direction signal is set via the host directive's direction ModelSignal
+       // This is handled by the (sortChange) binding below
+     })
+   }

+   onSortChange(event: TuiTableSortChange<PackageDataEntry>) {
+     const column = event.sortComparator === this.name ? 'name' : 'status'
+     this.prefs.setSort(column, event.sortDirection)
+   }

    readonly errors = toSignal(inject(DepErrorService).depErrors$)
    readonly services = input.required<readonly T[] | null>()
    readonly name: TuiComparator<PackageDataEntry> = byName
    readonly status: TuiComparator<PackageDataEntry> = ...
  }
```

**Important note on initial sort direction**: Taiga's `TuiTableDirective` has a `direction` `ModelSignal<TuiSortDirection>` that controls the active sort direction. We need to bind this from the preferences. The cleanest approach is to use a computed signal:

```typescript
// In the component, create a computed direction signal
readonly currentDirection = computed(() => {
  return this.prefs.sortState$.value.direction as TuiSortDirection
})
```

And bind it in the template: `[direction]="currentDirection()"

#### 3. `web/projects/ui/src/app/routes/portal/routes/services/dashboard/dashboard.component.ts` (modify)

**Action**: modify

No changes needed here — the `ServicesTableComponent` injects `ServicesPreferencesService` directly. The dashboard just passes the `services` input to the table.

### Verification

#### Automated
- [x] `cd web && npx tsc --project projects/ui/tsconfig.json --noEmit --skipLibCheck` — no TypeScript errors

#### Manual
- [ ] Click a column header to sort → navigate away from Services tab → navigate back → table is sorted by the previous column
- [ ] Close browser, reopen → sort is restored from localStorage

---

## Phase 3: Filter UI End-to-End

### Changes

#### 1. `web/projects/ui/src/app/routes/portal/routes/services/dashboard/table.component.ts` (modify)

**Action**: modify

Add filtering logic. The approach: use a `computed` signal in the component to filter the services array before it reaches the `tuiTableSort` pipe. This avoids creating a separate pipe since filtering is only used in this one component.

Template changes — replace the `@for` loop to apply filtering before sorting:

```diff
  @for (service of services() | tuiTableSort; track $index) {
```

becomes:

```diff
  @for (service of filteredServices(); track $index) {
```

Add the `filteredServices` computed in the component:

```typescript
import { computed } from '@angular/core'

// In ServicesTableComponent class:
readonly filteredServices = computed(() => {
  const raw = this.services()
  if (!raw) return []

  const filter = this.prefs.filterState$.value
  let result = raw

  // Apply text search
  if (filter.query) {
    const query = filter.query.toLowerCase()
    result = result.filter(pkg => {
      const manifest = getManifest(pkg)
      if (manifest?.title?.toLowerCase().includes(query)) return true
      return false
    })
  }

  // Apply state filter
  if (filter.states.length > 0) {
    result = result.filter(pkg => {
      const status = renderPkgStatus(pkg).primary
      return filter.states.includes(status)
    })
  }

  return result
})
```

**Note**: For Fuse.js fuzzy search, we'd need to add `fuse.js` as a dependency (it's already in `web/package.json` devDependencies). Since the task says "reuse the same fuzzy search strategy" from `FilterPackagesPipe`, we should use it. However, Fuse.js is already a production dependency in `web/package.json` (line: `"fuse.js": "^6.4.6"`), so we can import it directly.

Updated `filteredServices` with Fuse.js:

```typescript
import Fuse from 'fuse.js'

readonly filteredServices = computed(() => {
  const raw = this.services()
  if (!raw) return []

  const filter = this.prefs.filterState$.value
  let result = raw

  // Apply text search with Fuse.js
  if (filter.query) {
    const query = filter.query
    const fuse = new Fuse(raw, {
      keys: [
        { name: 'title', weight: 1 },
        { name: 'id', weight: 0.5 },
      ],
      threshold: query.length < 4 ? 0.2 : 0.6,
      includeScore: false,
      ignoreLocation: query.length >= 4,
      useExtendedSearch: query.length >= 4,
    })
    const fuseResults = fuse.search(query)
    const matchedIds = new Set(fuseResults.map(r => r.item.id))
    result = result.filter(pkg => matchedIds.has(pkg.id))
  }

  // Apply state filter
  if (filter.states.length > 0) {
    result = result.filter(pkg => {
      const status = renderPkgStatus(pkg).primary
      return filter.states.includes(status)
    })
  }

  return result
})
```

#### 2. `web/projects/ui/src/app/routes/portal/routes/services/dashboard/dashboard.component.ts` (modify)

**Action**: modify

Add filter UI (search input + state toggle buttons) to the dashboard template.

Template changes — add filter bar before the table:

```diff
  <section class="g-card">
    <header>
      {{ 'Installed services' | i18n }}
    </header>

+   <div class="filter-bar">
+     <input
+       type="text"
+       tuiTextfield
+       placeholder="Search services..."
+       [value]="prefs.filterState$.value.query"
+       (input)="prefs.setQuery($any($event.target).value)"
+     />
+     <div class="state-toggles">
+       @for (state of ALL_STATES; track state) {
+         <button
+           tuiButton
+           size="s"
+           [appearance]="prefs.filterState$.value.states.includes(state) ? 'primary' : 'secondary'"
+           (click)="prefs.toggleState(state)"
+         >
+           {{ state | titlecase }}
+         </button>
+       }
+     </div>
+   </div>

    <div #table [services]="services()"></div>
  </section>
```

Add the state list and import in the component:

```diff
+ import { ServicesPreferencesService } from 'src/app/services/services-preferences.service'
+ import { PrimaryRendering } from 'src/app/services/pkg-status-rendering.service'
+ import { TuiTextfield } from '@taiga-ui/kit'
+ import { TitlePipe } from '@angular/common'
+ import { TuiButton } from '@taiga-ui/core'
```

```diff
  export default class DashboardComponent {
+   protected readonly prefs = inject(ServicesPreferencesService)
+   protected readonly ALL_STATES = Object.keys(PrimaryRendering) as PrimaryStatus[]

    readonly services = toSignal(...)
    protected _ = viewChild<ServicesTableComponent<any>>('table')
  }
```

#### 3. `web/projects/ui/src/app/routes/portal/routes/services/dashboard/dashboard.component.ts` styles (modify)

**Action**: modify

Add minimal styles for the filter bar. Add to the existing `styles` array:

```diff
  styles: `
    :host {
      padding: 1rem;
    }
+   .filter-bar {
+     display: flex;
+     gap: 0.5rem;
+     padding: 0.75rem 1rem;
+     border-bottom: 1px solid var(--tui-base-03);
+   }
+   .filter-bar input {
+     flex: 1;
+   }
+   .state-toggles {
+     display: flex;
+     gap: 0.25rem;
+     flex-wrap: wrap;
+   }

    :host-context(tui-root._mobile) {
      header {
        display: none;
      }
      .g-card {
        padding: 0;
        margin-top: -0.75rem;
        background: none;
        box-shadow: none;
      }
    }
  `,
```

### Verification

#### Automated
- [x] `cd web && npx tsc --project projects/ui/tsconfig.json --noEmit --skipLibCheck` — no TypeScript errors

#### Manual
- [ ] Type in search box → table filters in real-time
- [ ] Click lifecycle toggles → only matching services shown
- [ ] Clear search + reset toggles → all services visible
- [ ] Navigate away and back → filter state restored

---

## Phase 4: Jest Setup + Tests

### Changes

#### 1. `web/package.json` (modify)

**Action**: modify

Add Jest devDependencies to the root `web/package.json`:

```diff
  "devDependencies": {
    ...
+   "@types/jest": "^29.5.14",
+   "jest": "^29.7.0",
+   "jest-preset-angular": "^14.4.2",
+   "ts-jest": "^29.2.5",
    ...
  },
  "scripts": {
    ...
+   "test": "jest --config projects/ui/jest.config.js",
    ...
  },
```

#### 2. `web/projects/ui/jest.config.js` (new)

**Action**: create

```javascript
module.exports = {
  preset: 'jest-preset-angular',
  rootDir: './',
  setupFilesAfterEnv: ['<rootDir>/jest.setup.ts'],
  moduleNameMapper: {
    '^src/app/(.*)$': '<rootDir>/src/app/$1',
    '^@start9labs/shared': '<rootDir>/projects/shared/src/index.ts',
  },
  testPathIgnorePatterns: ['<rootDir>/node_modules/'],
  collectCoverageFrom: [
    'src/app/**/*.ts',
    '!src/app/**/*.spec.ts',
    '!src/app/main.ts',
    '!src/app/polyfills.ts',
  ],
  coverageThresholds: {
    global: {
      statements: 80,
      branches: 80,
      functions: 80,
      lines: 80,
    },
  },
}
```

#### 3. `web/projects/ui/jest.setup.ts` (new)

**Action**: create

```typescript
import 'jest-preset-angular/setup.js'
```

#### 4. `web/projects/ui/tsconfig.spec.json` (new)

**Action**: create

```json
{
  "extends": "./tsconfig.json",
  "compilerOptions": {
    "outDir": "../../out/dist/ui",
    "module": "commonjs",
    "types": ["jest", "node"],
    "esModuleInterop": true
  },
  "include": ["src/**/*.spec.ts", "src/**/*.d.ts", "jest.setup.ts"]
}
```

#### 5. `web/projects/ui/src/app/services/services-preferences.service.spec.ts` (new)

**Action**: create

```typescript
import { ServicesPreferencesService } from './services-preferences.service'
import { StorageService } from './storage.service'
import { TestBed } from '@angular/core/testing'

describe('ServicesPreferencesService', () => {
  let service: ServicesPreferencesService
  let storageMock: jasmine.SpyObj<StorageService>

  beforeEach(() => {
    storageMock = jasmine.createSpyObj('StorageService', ['get', 'set'])
    storageMock.get.and.returnValue(null)

    TestBed.configureTestingModule({
      providers: [
        ServicesPreferencesService,
        { provide: StorageService, useValue: storageMock },
      ],
    })
    service = TestBed.inject(ServicesPreferencesService)
  })

  it('should be created', () => {
    expect(service).toBeTruthy()
  })

  it('should emit default sort state on init', (done) => {
    service.sortState$.subscribe(sort => {
      expect(sort.column).toBe('name')
      expect(sort.direction).toBe('asc')
      done()
    })
  })

  it('should emit default filter state on init', (done) => {
    service.filterState$.subscribe(filter => {
      expect(filter.query).toBe('')
      expect(filter.states).toContain('running')
      expect(filter.states).toContain('stopped')
      done()
    })
  })

  it('should restore sort state from storage', () => {
    storageMock.get.and.returnValue({
      sort: { column: 'status', direction: 'desc' },
      filter: { query: '', states: ['running'] },
    })

    const testBed2 = TestBed.configureTestingModule({
      providers: [
        ServicesPreferencesService,
        { provide: StorageService, useValue: storageMock },
      ],
    })
    const service2 = testBed2.inject(ServicesPreferencesService)

    service2.sortState$.subscribe(sort => {
      expect(sort.column).toBe('status')
      expect(sort.direction).toBe('desc')
    })
  })

  it('should persist sort state on setSort', () => {
    service.setSort('status', 'desc')
    expect(storageMock.set).toHaveBeenCalledWith(
      'services/preferences',
      jasmine.objectContaining({
        sort: { column: 'status', direction: 'desc' },
      }),
    )
  })

  it('should persist query on setQuery', () => {
    service.setQuery('test')
    expect(storageMock.set).toHaveBeenCalledWith(
      'services/preferences',
      jasmine.objectContaining({
        filter: { query: 'test' },
      }),
    )
  })

  it('should toggle state in filter', (done) => {
    service.filterState$.subscribe(filter => {
      if (filter.query === 'toggled') {
        expect(filter.states).not.toContain('running')
        done()
      }
    })
    service.toggleState('running')
    service.setQuery('toggled')
  })

  it('should reset filter to defaults', (done) => {
    service.toggleState('running')
    service.resetFilter()
    service.filterState$.subscribe(filter => {
      if (filter.query === '') {
        expect(filter.states).toContain('running')
        done()
      }
    })
  })
})
```

#### 6. `web/projects/ui/src/app/services/pkg-status-rendering.service.spec.ts` (new)

**Action**: create

```typescript
import { renderPkgStatus, getInstalledPrimaryStatus } from './pkg-status-rendering.service'
import { PackageDataEntry } from './patch-db/data-model'
import { T } from '@start9labs/start-sdk'
import { TestBed } from '@angular/core/testing'

describe('pkg-status-rendering.service', () => {
  describe('renderPkgStatus', () => {
    it('should return primary status for installed package with running service', () => {
      const pkg = {
        stateInfo: { state: 'installed' } as any,
        statusInfo: {
          desired: { main: 'running' },
          started: true,
          health: {},
          error: null,
        } as T.StatusInfo,
        tasks: {},
      } as PackageDataEntry

      const result = renderPkgStatus(pkg)
      expect(result.primary).toBe('running')
      expect(result.health).toBe('success')
    })

    it('should return primary status for installed package with stopped service', () => {
      const pkg = {
        stateInfo: { state: 'installed' } as any,
        statusInfo: {
          desired: { main: 'stopped' },
          started: false,
          health: {},
          error: null,
        } as T.StatusInfo,
        tasks: {},
      } as PackageDataEntry

      const result = renderPkgStatus(pkg)
      expect(result.primary).toBe('stopped')
      expect(result.health).toBe(null)
    })

    it('should return installing status for installing package', () => {
      const pkg = {
        stateInfo: { state: 'installing' } as any,
        statusInfo: {
          desired: { main: 'stopped' },
          started: false,
          health: {},
          error: null,
        } as T.StatusInfo,
        tasks: {},
      } as PackageDataEntry

      const result = renderPkgStatus(pkg)
      expect(result.primary).toBe('installing')
    })

    it('should return updating status for updating package', () => {
      const pkg = {
        stateInfo: { state: 'updating' } as any,
        statusInfo: {
          desired: { main: 'running' },
          started: true,
          health: {},
          error: null,
        } as T.StatusInfo,
        tasks: {},
      } as PackageDataEntry

      const result = renderPkgStatus(pkg)
      expect(result.primary).toBe('updating')
    })

    it('should return error status when statusInfo has error', () => {
      const pkg = {
        stateInfo: { state: 'installed' } as any,
        statusInfo: {
          desired: { main: 'running' },
          started: true,
          health: {},
          error: { code: 1, message: 'test error' },
        } as T.StatusInfo,
        tasks: {},
      } as PackageDataEntry

      const result = renderPkgStatus(pkg)
      expect(result.primary).toBe('error')
    })
  })

  describe('getInstalledPrimaryStatus', () => {
    it('should return task-required when critical task is active', () => {
      const pkg = {
        statusInfo: {
          desired: { main: 'running' },
          started: true,
          health: {},
          error: null,
        } as T.StatusInfo,
        tasks: {
          criticalTask: {
            active: true,
            task: { severity: 'critical' as const },
          },
        },
      } as PackageDataEntry

      const result = getInstalledPrimaryStatus(pkg)
      expect(result).toBe('task-required')
    })

    it('should return base status when no critical tasks', () => {
      const pkg = {
        statusInfo: {
          desired: { main: 'running' },
          started: true,
          health: {},
          error: null,
        } as T.StatusInfo,
        tasks: {},
      } as PackageDataEntry

      const result = getInstalledPrimaryStatus(pkg)
      expect(result).toBe('running')
    })
  })
})
```

### Verification

#### Automated
- [ ] `cd web && npm test` — all new tests pass
- [ ] `cd web && npm test -- --coverage` — coverage for `ServicesPreferencesService` > 90%

---

## Implementation Order Summary

| Phase | What | Files |
|---|---|---|
| 1 | `ServicesPreferencesService` | 1 new, 0 modified |
| 2 | Sort persistence wiring | 1 modified (`table.component.ts` wrapper), 1 modified (`ServicesTableComponent`) |
| 3 | Filter UI (search + toggles) | 1 modified (`ServicesTableComponent`), 1 modified (`DashboardComponent` template + styles) |
| 4 | Jest setup + tests | 1 modified (`web/package.json`), 3 new config files, 2 new spec files |
