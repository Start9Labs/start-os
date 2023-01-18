import { Injectable } from '@angular/core'
import {
  map,
  shareReplay,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from './patch-db/data-model'
import { ApiService } from './api/embassy-api.service'
import { combineLatest, from, timer } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class TimeService {
  private readonly startTimeMs$ = this.patch
    .watch$('server-info', 'system-start-time')
    .pipe(
      take(1),
      map(startTime => new Date(startTime).valueOf()),
      shareReplay(),
    )

  readonly systemTime$ = from(this.apiService.getSystemTime({})).pipe(
    switchMap(utcStr => {
      const dateObj = new Date(utcStr)
      const msRemaining = (60 - dateObj.getSeconds()) * 1000
      dateObj.setSeconds(0)
      const current = dateObj.valueOf()
      return timer(msRemaining, 60000).pipe(
        map(index => {
          const incremented = index + 1
          const msToAdd = 60000 * incremented
          return current + msToAdd
        }),
        startWith(current),
      )
    }),
  )

  readonly systemUptime$ = combineLatest([
    this.startTimeMs$,
    this.systemTime$,
  ]).pipe(
    map(([startTime, currentTime]) => {
      const ms = currentTime - startTime
      const days = Math.floor(ms / (24 * 60 * 60 * 1000))
      const daysms = ms % (24 * 60 * 60 * 1000)
      const hours = Math.floor(daysms / (60 * 60 * 1000))
      const hoursms = ms % (60 * 60 * 1000)
      const minutes = Math.floor(hoursms / (60 * 1000))
      return { days, hours, minutes }
    }),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly apiService: ApiService,
  ) {}
}
