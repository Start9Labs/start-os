import { Component, Input, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { RR } from 'src/app/services/api/api.types'
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
  templateUrl: './logs.page.html',
  styleUrls: ['./logs.page.scss'],
})
export class LogsPage {
  @ViewChild(IonContent)
  private content?: IonContent

  @Input()
  fetchLogs!: (params: {
    before_flag?: boolean
    limit?: number
    cursor?: string
  }) => Promise<RR.LogsRes>

  loading = true
  loadingNext = false
  needInfinite = true
  startCursor?: string
  endCursor?: string
  limit = 400
  isOnBottom = true

  constructor(private readonly errToast: ErrorToastService) {}

  async ngOnInit() {
    await this.getPrior()
    this.loading = false
  }

  async getNext() {
    this.loadingNext = true
    const logs = await this.fetch(false)
    if (!logs?.length) return (this.loadingNext = false)

    const container = document.getElementById('container')
    const newLogs = document.getElementById('template')?.cloneNode(true)

    if (!(newLogs instanceof HTMLElement)) return

    newLogs.innerHTML =
      logs.map(l => `${l.timestamp} ${convert.toHtml(l.message)}`).join('\n') +
      (logs.length ? '\n' : '')
    container?.append(newLogs)
    this.loadingNext = false
    this.scrollEvent()
  }

  async doInfinite(e: any): Promise<void> {
    await this.getPrior()
    e.target.complete()
  }

  scrollEvent() {
    const buttonDiv = document.getElementById('button-div')
    this.isOnBottom =
      !!buttonDiv && buttonDiv.getBoundingClientRect().top < window.innerHeight
  }

  scrollToBottom() {
    this.content?.scrollToBottom(500)
  }

  private async getPrior() {
    // get logs
    const logs = await this.fetch()
    if (!logs?.length) return

    const container = document.getElementById('container')
    const beforeContainerHeight = container?.scrollHeight || 0
    const newLogs = document.getElementById('template')?.cloneNode(true)

    if (!(newLogs instanceof HTMLElement)) return

    newLogs.innerHTML =
      logs.map(l => `${l.timestamp} ${convert.toHtml(l.message)}`).join('\n') +
      (logs.length ? '\n' : '')
    container?.prepend(newLogs)
    const afterContainerHeight = container?.scrollHeight || 0

    // scroll down
    scrollBy(0, afterContainerHeight - beforeContainerHeight)
    this.content?.scrollToPoint(0, afterContainerHeight - beforeContainerHeight)

    if (logs.length < this.limit) {
      this.needInfinite = false
    }
  }

  private async fetch(isBefore: boolean = true) {
    try {
      const cursor = isBefore ? this.startCursor : this.endCursor
      const logsRes = await this.fetchLogs({
        cursor,
        before_flag: !!cursor ? isBefore : undefined,
        limit: this.limit,
      })

      if ((isBefore || this.startCursor) && logsRes['start-cursor']) {
        this.startCursor = logsRes['start-cursor']
      }

      if ((!isBefore || !this.endCursor) && logsRes['end-cursor']) {
        this.endCursor = logsRes['end-cursor']
      }

      return logsRes.entries
    } catch (e: any) {
      this.errToast.present(e)
    }
  }
}
