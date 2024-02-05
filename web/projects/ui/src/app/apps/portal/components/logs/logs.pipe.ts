import { inject, Pipe, PipeTransform } from '@angular/core'
import { convertAnsi, toLocalIsoString } from '@start9labs/shared'
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
  standalone: true,
})
export class LogsPipe implements PipeTransform {
  private readonly api = inject(ApiService)
  private readonly logs = inject(LogsComponent)
  private readonly connection = inject(ConnectionService)

  transform(
    followLogs: (
      params: RR.FollowServerLogsReq,
    ) => Promise<RR.FollowServerLogsRes>,
  ): Observable<readonly string[]> {
    return merge(
      this.logs.status$.pipe(
        skipWhile(value => value === 'connected'),
        filter(value => value === 'connected'),
        map(() => getMessage(true)),
      ),
      defer(() => followLogs(this.options)).pipe(
        tap(r => this.logs.setCursor(r['start-cursor'])),
        switchMap(r => this.api.openLogsWebsocket$(this.toConfig(r.guid))),
        bufferTime(1000),
        filter(logs => !!logs.length),
        map(convertAnsi),
      ),
    ).pipe(
      catchError(() =>
        this.connection.connected$.pipe(
          tap(v => this.logs.status$.next(v ? 'reconnecting' : 'disconnected')),
          filter(Boolean),
          take(1),
          ignoreElements(),
          startWith(getMessage(false)),
        ),
      ),
      repeat(),
      scan((logs: string[], log) => [...logs, log], []),
    )
  }

  private get options() {
    return this.logs.status$.value === 'connected' ? { limit: 400 } : {}
  }

  private toConfig(guid: string) {
    return {
      url: `/rpc/${guid}`,
      openObserver: {
        next: () => this.logs.status$.next('connected'),
      },
    }
  }
}

function getMessage(success: boolean): string {
  return `<p style="color: ${
    success ? 'var(--tui-success-fill)' : 'var(--tui-error-fill)'
  }; text-align: center;">${
    success ? 'Reconnected' : 'Disconnected'
  } at ${toLocalIsoString(new Date())}</p>`
}
