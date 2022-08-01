import { DOCUMENT } from '@angular/common'
import { Component, Inject, Input, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import { takeUntil } from 'rxjs'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import {
  LogsRes,
  ServerLogsReq,
  DestroyService,
  ErrorToastService,
  toLocalIsoString,
} from '@start9labs/shared'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

var Convert = require('ansi-to-html')
var convert = new Convert({
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
  providers: [DestroyService],
})
export class LogsComponent {
  @ViewChild(IonContent)
  private content?: IonContent

  @Input()
  tailLogs!: (params: RR.TailServerLogsReq) => Promise<RR.TailServerLogsRes>

  @Input()
  fetchLogs!: (params: ServerLogsReq) => Promise<LogsRes>

  loading = true
  needInfinite = true
  startCursor?: string
  guid?: string
  limit = 400
  isOnBottom = true
  autoScroll = true
  websocketFail = false

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly errToast: ErrorToastService,
    private readonly destroy$: DestroyService,
    private readonly api: ApiService,
  ) {}

  async ngOnInit() {
    try {
      const { 'start-cursor': startCursor, guid } = await this.tailLogs({
        limit: this.limit,
      })

      this.startCursor = startCursor

      const host = this.document.location.host
      const protocol =
        this.document.location.protocol === 'http:' ? 'ws' : 'wss'

      const config: WebSocketSubjectConfig<LogsRes> = {
        url: `${protocol}://${host}/ws/rpc/${guid}`,
        openObserver: {
          next: () => {
            this.loading = false
            this.websocketFail = false
          },
        },
      }

      this.api
        .openLogsWebsocket$(config)
        .pipe(takeUntil(this.destroy$))
        .subscribe({
          next: msg => {
            this.processRes(msg, false)
          },
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

      this.processRes(res, true)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      e.target.complete()
    }
  }

  scrollEnd() {
    const bottomDiv = document.getElementById('bottom-div')
    this.isOnBottom =
      !!bottomDiv &&
      bottomDiv.getBoundingClientRect().top - 420 < window.innerHeight
  }

  scrollToBottom() {
    this.content?.scrollToBottom(250)
  }

  private async processRes(res: LogsRes, before: boolean) {
    const { entries, 'start-cursor': startCursor } = res

    if (!entries.length) return

    const container = document.getElementById('container')
    const newLogs = document.getElementById('template')?.cloneNode(true)

    if (!(newLogs instanceof HTMLElement)) return

    newLogs.innerHTML = entries
      .map(
        entry =>
          `<b>${toLocalIsoString(
            new Date(entry.timestamp),
          )}</b> ${convert.toHtml(entry.message)}`,
      )
      .join('\n')

    if (before) {
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
      }, 50)

      if (entries.length < this.limit) {
        this.needInfinite = false
      }
    } else {
      container?.append(newLogs)
      if (this.autoScroll) {
        // scroll to bottom
        setTimeout(() => {
          this.scrollToBottom()
        }, 50)
      }
    }
  }
}
