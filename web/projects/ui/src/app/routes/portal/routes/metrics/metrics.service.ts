import { inject, Injectable } from '@angular/core'
import {
  catchError,
  defer,
  filter,
  ignoreElements,
  Observable,
  repeat,
  retry,
  shareReplay,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs'
import { ServerMetrics } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConnectionService } from 'src/app/services/connection.service'

@Injectable({
  providedIn: 'root',
})
export class MetricsService extends Observable<ServerMetrics> {
  private readonly connection = inject(ConnectionService)
  private readonly api = inject(ApiService)

  private readonly metrics$ = defer(() =>
    this.api.followServerMetrics({}),
  ).pipe(
    switchMap(({ guid, metrics }) =>
      this.api.openWebsocket$<ServerMetrics>(guid).pipe(startWith(metrics)),
    ),
    catchError(() =>
      this.connection.pipe(filter(Boolean), take(1), ignoreElements()),
    ),
    repeat(),
    shareReplay(1),
  )

  constructor() {
    super(subscriber => this.metrics$.subscribe(subscriber))
  }
}
