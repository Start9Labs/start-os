import { Component, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import {
  DownloadHTMLService,
  ErrorService,
  LoadingService,
  Log,
  toLocalIsoString,
} from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'

var Convert = require('ansi-to-html')
var convert = new Convert({
  bg: 'transparent',
})

@Component({
  selector: 'logs',
  templateUrl: './logs.page.html',
  styleUrls: ['./logs.page.scss'],
})
export class LogsPage {
  @ViewChild(IonContent) private content?: IonContent
  loading = true
  needInfinite = true
  startCursor?: string
  limit = 400
  isOnBottom = true

  constructor(
    private readonly api: ApiService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly downloadHtml: DownloadHTMLService,
  ) {}

  async ngOnInit() {
    await this.getLogs()
    this.loading = false
  }

  scrollEnd() {
    const bottomDiv = document.getElementById('bottom-div')
    this.isOnBottom =
      !!bottomDiv &&
      bottomDiv.getBoundingClientRect().top - 420 < window.innerHeight
  }

  scrollToBottom() {
    this.content?.scrollToBottom(500)
  }

  async doInfinite(e: any): Promise<void> {
    await this.getLogs()
    e.target.complete()
  }

  async download() {
    const loader = this.loader.open('Processing 10,000 logs...').subscribe()

    try {
      const { entries } = await this.api.diagnosticGetLogs({
        before: true,
        limit: 10000,
      })

      const styles = {
        'background-color': '#222428',
        color: '#e0e0e0',
        'font-family': 'monospace',
      }
      const html = this.convertToAnsi(entries)

      this.downloadHtml.download('diagnostic-logs.html', html, styles)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async getLogs() {
    try {
      const { startCursor, entries } = await this.api.diagnosticGetLogs({
        cursor: this.startCursor,
        before: !!this.startCursor,
        limit: this.limit,
      })

      if (!entries.length) return

      this.startCursor = startCursor

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
    } catch (e: any) {
      this.errorService.handleError(e)
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
