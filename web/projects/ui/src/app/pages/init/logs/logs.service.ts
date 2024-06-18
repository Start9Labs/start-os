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
  private readonly log$ = defer(() => this.api.initFollowLogs({})).pipe(
    switchMap(({ guid }) => this.api.openWebsocket$<Log>(guid, {})),
    bufferTime(250),
    filter(logs => !!logs.length),
    map(convertAnsi),
    scan((logs: readonly string[], log) => [...logs, log], []),
  )

  constructor() {
    super(subscriber => this.log$.subscribe(subscriber))
  }
}
