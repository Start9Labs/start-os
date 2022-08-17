import { Component, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { ErrorToastService, toLocalIsoString } from '@start9labs/shared'

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
  limit = 200
  isOnBottom = true

  constructor(
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
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

  private async getLogs() {
    try {
      const { 'start-cursor': startCursor, entries } = await this.api.getLogs({
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
      this.errToast.present(e)
    }
  }
}
