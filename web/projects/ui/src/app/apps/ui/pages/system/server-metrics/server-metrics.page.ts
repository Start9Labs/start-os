import { Component } from '@angular/core'
import { Metrics } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TimeService } from 'src/app/services/time-service'
import {
  catchError,
  combineLatest,
  filter,
  from,
  Observable,
  startWith,
  switchMap,
} from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'

@Component({
  selector: 'server-metrics',
  templateUrl: './server-metrics.page.html',
  styleUrls: ['./server-metrics.page.scss'],
})
export class ServerMetricsPage {
  websocketFail = false

  readonly serverData$ = this.getServerData$()

  constructor(
    private readonly api: ApiService,
    private readonly timeService: TimeService,
    private readonly connectionService: ConnectionService,
  ) {}

  private getServerData$(): Observable<
    [
      {
        value: number
        synced: boolean
      },
      {
        days: number
        hours: number
        minutes: number
        seconds: number
      },
      Metrics,
    ]
  > {
    return combineLatest([
      this.timeService.now$,
      this.timeService.uptime$,
      this.getMetrics$(),
    ]).pipe(
      catchError(() => {
        this.websocketFail = true
        return this.connectionService.connected$.pipe(
          filter(Boolean),
          switchMap(() => this.getServerData$()),
        )
      }),
    )
  }

  private getMetrics$(): Observable<Metrics> {
    return from(this.api.getServerMetrics({})).pipe(
      switchMap(({ metrics, guid }) =>
        this.api
          .openMetricsWebsocket$({
            url: `/rpc/${guid}`,
            openObserver: {
              next: () => (this.websocketFail = false),
            },
          })
          .pipe(startWith(metrics)),
      ),
    )
  }
}
