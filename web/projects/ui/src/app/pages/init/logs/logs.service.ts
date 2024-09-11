import { inject, Injectable } from '@angular/core'
import { Log, toLocalIsoString } from '@start9labs/shared'
import {
  bufferTime,
  defer,
  filter,
  map,
  Observable,
  scan,
  switchMap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { retryWithState } from 'src/app/util/retry-with-state'

var Convert = require('ansi-to-html')
var convert = new Convert({
  newline: true,
  bg: 'transparent',
  colors: {
    4: 'Cyan',
  },
  escapeXML: true,
})

function convertAnsi(entries: readonly any[]): string {
  return entries
    .map(
      ({ timestamp, message }) =>
        `<b style="color: #FFF">${toLocalIsoString(
          new Date(timestamp),
        )}</b>&nbsp;&nbsp;${convert.toHtml(message)}`,
    )
    .join('<br />')
}

@Injectable({ providedIn: 'root' })
export class LogsService extends Observable<readonly string[]> {
  private readonly api = inject(ApiService)
  // @TODO Matt — when we retry, should we use different `boot`?
  private readonly log$ = defer(() =>
    this.api.initFollowLogs({ boot: 0 }),
  ).pipe(
    switchMap(({ guid }) => this.api.openWebsocket$<Log>(guid)),
    bufferTime(500),
    filter(logs => !!logs.length),
    map(convertAnsi),
    scan((logs: readonly string[], log) => [...logs, log], []),
    retryWithState(),
  )

  constructor() {
    super(subscriber => this.log$.subscribe(subscriber))
  }
}