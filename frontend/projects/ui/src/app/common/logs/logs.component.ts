import { Component, Input, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import {
  bufferTime,
  catchError,
  filter,
  finalize,
  from,
  Observable,
  switchMap,
  takeUntil,
  tap,
} from 'rxjs'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import {
  LogsRes,
  ServerLogsReq,
  toLocalIsoString,
  Log,
  DownloadHTMLService,
  LoadingService,
  ErrorService,
} from '@start9labs/shared'
import { TuiDestroyService } from '@taiga-ui/cdk'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConnectionService } from 'src/app/services/connection.service'

var Convert = require('ansi-to-html')
var convert = new Convert({
  newline: true,
  bg: 'transparent',
  colors: {
    4: 'Cyan',
  },
  escapeXML: true,
})

@Component({
  selector: 'logs',
  templateUrl: './logs.component.html',
  styleUrls: ['./logs.component.scss'],
  providers: [TuiDestroyService, DownloadHTMLService],
})
export class LogsComponent {
  @ViewChild(IonContent)
  private content?: IonContent

  @Input() followLogs!: (
    params: RR.FollowServerLogsReq,
  ) => Promise<RR.FollowServerLogsRes>
  @Input() fetchLogs!: (params: ServerLogsReq) => Promise<LogsRes>
  @Input() context!: string
  @Input() defaultBack!: string
  @Input() pageTitle!: string

  loading = true
  infiniteStatus: 0 | 1 | 2 = 0
  startCursor?: string
  isOnBottom = true
  autoScroll = true
  websocketStatus:
    | 'connecting'
    | 'connected'
    | 'reconnecting'
    | 'disconnected' = 'connecting'
  limit = 400
  count = 0

  constructor(
    private readonly errorService: ErrorService,
    private readonly destroy$: TuiDestroyService,
    private readonly api: ApiService,
    private readonly loader: LoadingService,
    private readonly downloadHtml: DownloadHTMLService,
    private readonly connectionService: ConnectionService,
  ) {}

  async ngOnInit() {
    from(this.followLogs({ limit: this.limit }))
      .pipe(
        switchMap(({ 'start-cursor': startCursor, guid }) => {
          this.startCursor = startCursor
          return this.connect$(guid)
        }),
        takeUntil(this.destroy$),
        finalize(() => console.log('CLOSING')),
      )
      .subscribe()
  }

  async doInfinite(e: any): Promise<void> {
    try {
      const res = await this.fetchLogs({
        cursor: this.startCursor,
        before: true,
        limit: this.limit,
      })

      this.processRes(res)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      e.target.complete()
    }
  }

  handleScroll(e: any) {
    if (e.detail.deltaY < -50) this.autoScroll = false
  }

  handleScrollEnd() {
    const bottomDiv = document.getElementById('bottom-div')
    this.isOnBottom =
      !!bottomDiv &&
      bottomDiv.getBoundingClientRect().top - 420 < window.innerHeight
  }

  scrollToBottom() {
    this.content?.scrollToBottom(200)
  }

  async download() {
    const loader = this.loader.open('Processing 10,000 logs...').subscribe()

    try {
      const { entries } = await this.fetchLogs({
        before: true,
        limit: 10000,
      })

      const styles = {
        'background-color': '#222428',
        color: '#e0e0e0',
        'font-family': 'monospace',
      }
      const html = this.convertToAnsi(entries)

      this.downloadHtml.download(`${this.context}-logs.html`, html, styles)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private reconnect$(): Observable<Log[]> {
    return from(this.followLogs({})).pipe(
      tap(_ => this.recordConnectionChange()),
      switchMap(({ guid }) => this.connect$(guid, true)),
    )
  }

  private connect$(guid: string, reconnect = false) {
    const config: WebSocketSubjectConfig<Log> = {
      url: `/rpc/${guid}`,
      openObserver: {
        next: () => {
          this.websocketStatus = 'connected'
        },
      },
    }

    return this.api.openLogsWebsocket$(config).pipe(
      tap(_ => this.count++),
      bufferTime(1000),
      tap(msgs => {
        this.loading = false
        this.processRes({ entries: msgs })
        if (this.infiniteStatus === 0 && this.count >= this.limit)
          this.infiniteStatus = 1
      }),
      catchError(() => {
        this.recordConnectionChange(false)
        return this.connectionService.connected$.pipe(
          tap(
            connected =>
              (this.websocketStatus = connected
                ? 'reconnecting'
                : 'disconnected'),
          ),
          filter(Boolean),
          switchMap(() => this.reconnect$()),
        )
      }),
    )
  }

  private recordConnectionChange(success = true) {
    const container = document.getElementById('container')
    const elem = document.getElementById('template')?.cloneNode()
    if (!(elem instanceof HTMLElement)) return
    elem.innerHTML = `<div style="padding: ${
      success ? '36px 0' : '36px 0 0 0'
    }; color: ${success ? '#2fdf75' : '#ff4961'}; text-align: center;">${
      success ? 'Reconnected' : 'Disconnected'
    } at ${toLocalIsoString(new Date())}</div>`
    container?.append(elem)
    if (this.isOnBottom) {
      setTimeout(() => {
        this.scrollToBottom()
      }, 25)
    }
  }

  private processRes(res: LogsRes) {
    const { entries, 'start-cursor': startCursor } = res

    if (!entries.length) return

    const container = document.getElementById('container')
    const newLogs = document.getElementById('template')?.cloneNode()

    if (!(newLogs instanceof HTMLElement)) return

    newLogs.innerHTML = this.convertToAnsi(entries)

    // if response contains a startCursor, it means we are scrolling backwards
    if (startCursor) {
      this.startCursor = startCursor

      const beforeContainerHeight = container?.scrollHeight || 0
      container?.prepend(newLogs)
      const afterContainerHeight = container?.scrollHeight || 0

      // maintain scroll height
      setTimeout(() => {
        this.content?.scrollToPoint(
          0,
          afterContainerHeight - beforeContainerHeight,
        )
      }, 25)

      if (entries.length < this.limit) {
        this.infiniteStatus = 2
      }
    } else {
      container?.append(newLogs)
      if (this.autoScroll) {
        setTimeout(() => {
          this.scrollToBottom()
        }, 25)
      }
    }
  }

  private convertToAnsi(entries: Log[]) {
    return entries
      .map(
        entry =>
          `<span style="color: #FFF; font-weight: bold;">${toLocalIsoString(
            new Date(entry.timestamp),
          )}</span>&nbsp;&nbsp;${convert.toHtml(entry.message)}`,
      )
      .join('<br />')
  }
}
