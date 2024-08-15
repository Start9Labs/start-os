import { inject, Injectable } from '@angular/core'
import {
  defer,
  Observable,
  retry,
  shareReplay,
  startWith,
  switchMap,
} from 'rxjs'
import { ServerMetrics } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class MetricsService extends Observable<ServerMetrics> {
  private readonly api = inject(ApiService)

  // @TODO Alex do we need to use defer? I am unsure when this is necessary.
  private readonly metrics$ = defer(() =>
    this.api.followServerMetrics({}),
  ).pipe(
    switchMap(({ guid, metrics }) =>
      this.api.openWebsocket$<ServerMetrics>(guid).pipe(startWith(metrics)),
    ),
    // @TODO Alex how to handle failure and reconnection here? Simple retry() will not work. Seems like we need a general solution for reconnecting websockets: patchDB, logs, metrics, progress, and any future. Reconnection should depend on server state, then we need to get a new guid, then reconnect. Similar to how patchDB websocket currently behaves on disconnect/reconnect.
    retry(),
    shareReplay(),
  )

  constructor() {
    super(subscriber => this.metrics$.subscribe(subscriber))
  }
}
