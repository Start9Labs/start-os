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
