import { Component, Input, ViewChild } from '@angular/core'
import { IonContent, LoadingController } from '@ionic/angular'
import { bufferTime, takeUntil, tap } from 'rxjs'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import {
  LogsRes,
  ServerLogsReq,
  DestroyService,
  ErrorToastService,
  toLocalIsoString,
  Log,
  DownloadHTMLService,
} from '@start9labs/shared'
import { RR } from 'src/app/services/api/api.types'
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

@Component({
  selector: 'logs',
  templateUrl: './logs.component.html',
  styleUrls: ['./logs.component.scss'],
  providers: [DestroyService, DownloadHTMLService],
})
export class LogsComponent {
  @ViewChild(IonContent)
  private content?: IonContent

  @Input() followLogs!: (
    params: RR.FollowServerLogsReq,
  ) => Promise<RR.FollowServerLogsRes>
  @Input() fetchLogs!: (params: ServerLogsReq) => Promise<LogsRes>
  @Input() defaultBack!: string
  @Input() title!: string

  loading = true
  needInfinite = false
  startCursor?: string
  isOnBottom = true
  autoScroll = true
  websocketFail = false
  limit = 400

  constructor(
    private readonly errToast: ErrorToastService,
    private readonly destroy$: DestroyService,
    private readonly api: ApiService,
    private readonly loadingCtrl: LoadingController,
    private readonly downloadHtml: DownloadHTMLService,
  ) {}

  async ngOnInit() {
    try {
      const { 'start-cursor': startCursor, guid } = await this.followLogs({
        limit: this.limit,
      })

      this.startCursor = startCursor

      const config: WebSocketSubjectConfig<Log> = {
        url: `/rpc/${guid}`,
        openObserver: {
          next: () => {
            this.websocketFail = false
          },
        },
      }

      let totalLogs = 0

      this.api
        .openLogsWebsocket$(config)
        .pipe(
          tap(_ => {
            totalLogs++
            if (totalLogs === this.limit) this.needInfinite = true
          }),
          bufferTime(500),
          tap(msgs => {
            this.loading = false
            this.processRes({ entries: msgs })
          }),
          takeUntil(this.destroy$),
        )
        .subscribe({
          error: () => {
            this.websocketFail = true
            if (this.isOnBottom) this.scrollToBottom()
          },
        })
    } catch (e: any) {
      this.errToast.present(e)
    }
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
      this.errToast.present(e)
    } finally {
      e.target.complete()
    }
  }

  handleScroll(e: any) {
    if (e.detail.deltaY < 0) this.autoScroll = false
  }

  handleScrollEnd() {
    const bottomDiv = document.getElementById('bottom-div')
    this.isOnBottom =
      !!bottomDiv &&
      bottomDiv.getBoundingClientRect().top - 420 < window.innerHeight
  }

  scrollToBottom() {
    this.content?.scrollToBottom(250)
  }

  async download() {
    const loader = await this.loadingCtrl.create({
      message: 'Processing 10,000 logs...',
    })
    await loader.present()

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

      this.downloadHtml.download('logs.html', html, styles)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
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

      // scroll down
      setTimeout(() => {
        this.content?.scrollToPoint(
          0,
          afterContainerHeight - beforeContainerHeight,
        )
      }, 25)

      if (entries.length < this.limit) {
        this.needInfinite = false
      }
    } else {
      container?.append(newLogs)
      if (this.autoScroll) {
        // scroll to bottom
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
