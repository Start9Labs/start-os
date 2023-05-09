import { Injectable } from '@angular/core'
import { map, startWith, switchMap } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from './patch-db/data-model'
import { ApiService } from './api/embassy-api.service'
import { combineLatest, from, Observable, timer } from 'rxjs'

export interface TimeInfo {
  systemStartTime: number
  systemCurrentTime: number
  systemUptime: {
    days: number
    hours: number
    minutes: number
    seconds: number
  }
}

@Injectable({
  providedIn: 'root',
})
export class TimeService {
  private readonly systemStartTime$ = this.patch
    .watch$('server-info', 'system-start-time')
    .pipe(map(startTime => new Date(startTime).valueOf()))

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly apiService: ApiService,
  ) {}

  getTimeInfo$(): Observable<TimeInfo> {
    return combineLatest([
      this.systemStartTime$.pipe(),
      this.getSystemCurrentTime$(),
    ]).pipe(
      map(([systemStartTime, systemCurrentTime]) => ({
        systemStartTime,
        systemCurrentTime,
        systemUptime: this.getSystemUptime(systemStartTime, systemCurrentTime),
      })),
    )
  }

  private getSystemCurrentTime$() {
    return from(this.apiService.getSystemTime({})).pipe(
      switchMap(utcStr => {
        const dateObj = new Date(utcStr)
        const current = dateObj.valueOf()
        return timer(0, 1000).pipe(
          map(index => {
            const incremented = index + 1
            const msToAdd = 1000 * incremented
            return current + msToAdd
          }),
          startWith(current),
        )
      }),
    )
  }

  private getSystemUptime(systemStartTime: number, systemCurrentTime: number) {
    const ms = systemCurrentTime - systemStartTime

    const days = Math.floor(ms / (24 * 60 * 60 * 1000))
    const daysms = ms % (24 * 60 * 60 * 1000)

    const hours = Math.floor(daysms / (60 * 60 * 1000))
    const hoursms = ms % (60 * 60 * 1000)

    const minutes = Math.floor(hoursms / (60 * 1000))
    const minutesms = ms % (60 * 1000)

    const seconds = Math.floor(minutesms / 1000)

    return { days, hours, minutes, seconds }
  }
}
