import { Injectable } from '@angular/core'
import { map, shareReplay, startWith, switchMap } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from './patch-db/data-model'
import { ApiService } from './api/embassy-api.service'
import { combineLatest, from, interval } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class TimeService {
  private readonly time$ = from(this.apiService.getSystemTime({})).pipe(
    switchMap(({ now, uptime }) => {
      const current = new Date(now).valueOf()
      return interval(1000).pipe(
        map(index => {
          const incremented = index + 1
          const msToAdd = 1000 * incremented
          return {
            now: current + msToAdd,
            uptime: uptime + msToAdd,
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
      const days = Math.floor(uptime / (24 * 60 * 60 * 1000))
      const daysms = uptime % (24 * 60 * 60 * 1000)
      const hours = Math.floor(daysms / (60 * 60 * 1000))
      const hoursms = uptime % (60 * 60 * 1000)
      const minutes = Math.floor(hoursms / (60 * 1000))
      const minutesms = uptime % (60 * 1000)
      const seconds = Math.floor(minutesms / 1000)
      return { days, hours, minutes, seconds }
    }),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly apiService: ApiService,
  ) {}
}
