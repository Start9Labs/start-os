import { Injectable } from '@angular/core'
import { map, shareReplay, startWith, switchMap } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from './patch-db/data-model'
import { ApiService } from './api/embassy-api.service'
import { combineLatest, interval, of } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class TimeService {
  private readonly time$ = of({}).pipe(
    switchMap(() => this.apiService.getSystemTime({})),
    switchMap(({ now, uptime }) => {
      const current = new Date(now).valueOf()
      return interval(1000).pipe(
        map(index => {
          const incremented = index + 1
          return {
            now: current + 1000 * incremented,
            uptime: uptime + incremented,
          }
        }),
        startWith({
          now: current,
          uptime,
        }),
      )
    }),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  readonly now$ = combineLatest([
    this.time$,
    this.patch.watch$('server-info', 'ntp-synced'),
  ]).pipe(
    map(([time, synced]) => ({
      value: time.now,
      synced,
    })),
  )

  readonly uptime$ = this.time$.pipe(
    map(({ uptime }) => {
      const days = Math.floor(uptime / (24 * 60 * 60))
      const daysSec = uptime % (24 * 60 * 60)
      const hours = Math.floor(daysSec / (60 * 60))
      const hoursSec = uptime % (60 * 60)
      const minutes = Math.floor(hoursSec / 60)
      const seconds = uptime % 60
      return { days, hours, minutes, seconds }
    }),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly apiService: ApiService,
  ) {}
}
