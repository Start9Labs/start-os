import { inject, Pipe, PipeTransform } from '@angular/core'
import {
  convertAnsi,
  i18nPipe,
  Log,
  toLocalIsoString,
} from '@start9labs/shared'
import {
  bufferTime,
  catchError,
  defer,
  filter,
  ignoreElements,
  map,
  merge,
  Observable,
  repeat,
  scan,
  skipWhile,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { LogsComponent } from './logs.component'

@Pipe({
  name: 'logs',
})
export class LogsPipe implements PipeTransform {
  private readonly api = inject(ApiService)
  private readonly logs = inject(LogsComponent)
  private readonly connection = inject(ConnectionService)
  private readonly i18n = inject(i18nPipe)

  transform(
    followLogs: (
      params: RR.FollowServerLogsReq,
    ) => Promise<RR.FollowServerLogsRes>,
  ): Observable<readonly string[]> {
    return merge(
      this.logs.status$.pipe(
        skipWhile(value => value === 'connected'),
        filter(value => value === 'connected'),
        map(() => this.getMessage(true)),
      ),
      defer(() => followLogs(this.options)).pipe(
        tap(r => this.logs.setCursor(r.startCursor)),
        switchMap(r =>
          this.api.openWebsocket$<Log>(r.guid, {
            openObserver: {
              next: () => this.logs.status$.next('connected'),
            },
          }),
        ),
        bufferTime(1000),
        filter(logs => !!logs.length),
        map(convertAnsi),
      ),
    ).pipe(
      catchError(() =>
        this.connection.pipe(
          tap(v => this.logs.status$.next(v ? 'reconnecting' : 'disconnected')),
          filter(Boolean),
          take(1),
          ignoreElements(),
          startWith(this.getMessage(false)),
        ),
      ),
      repeat(),
      scan((logs: string[], log) => [...logs, log], []),
    )
  }

  private getMessage(success: boolean): string {
    return `<p style="color: ${
      success ? 'var(--tui-status-positive)' : 'var(--tui-status-negative)'
    }; text-align: center;">${this.i18n.transform(
      success ? 'Reconnected' : 'Disconnected',
    )} at ${toLocalIsoString(new Date())}</p>`
  }

  private get options() {
    return this.logs.status$.value === 'connected' ? { limit: 400 } : {}
  }
}
