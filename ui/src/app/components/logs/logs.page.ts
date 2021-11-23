import { Component, Input, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { RR } from 'src/app/services/api/api.types'
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
  @ViewChild(IonContent) private content: IonContent
  @Input() fetchLogs: (params: { before_flag?: boolean, limit?: number, cursor?: string }) => Promise<RR.LogsRes>
  loading = true
  loadingMore = false
  logs: string
  needInfinite = true
  startCursor: string
  endCursor: string
  limit = 200
  scrollToBottomButton = false
  isOnBottom = true

  constructor (
    private readonly errToast: ErrorToastService,
  ) { }

  ngOnInit () {
    this.getLogs()
  }

  async fetch (isBefore: boolean = true) {
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
      this.loading = false

      return logsRes.entries
    } catch (e) {
      this.errToast.present(e)
    }
  }

  async getLogs () {
    try {
      // get logs
      const logs = await this.fetch()
      if (!logs.length) return

      const container = document.getElementById('container')
      const beforeContainerHeight = container.scrollHeight
      const newLogs = document.getElementById('template').cloneNode(true) as HTMLElement
      newLogs.innerHTML = logs.map(l => `${l.timestamp} ${convert.toHtml(l.message)}`).join('\n') + (logs.length ? '\n' : '')
      container.prepend(newLogs)
      const afterContainerHeight = container.scrollHeight

      // scroll down
      scrollBy(0, afterContainerHeight - beforeContainerHeight)
      this.content.scrollToPoint(0, afterContainerHeight - beforeContainerHeight)

      if (logs.length < this.limit) {
        this.needInfinite = false
      }

    } catch (e) { }
  }

  async loadMore () {
    try {
      this.loadingMore = true
      const logs = await this.fetch(false)
      if (!logs.length) return this.loadingMore = false

      const container = document.getElementById('container')
      const newLogs = document.getElementById('template').cloneNode(true) as HTMLElement
      newLogs.innerHTML = logs.map(l => `${l.timestamp} ${convert.toHtml(l.message)}`).join('\n') + (logs.length ? '\n' : '')
      container.append(newLogs)
      this.loadingMore = false
      this.scrollEvent()
    } catch (e) { }
  }

  scrollEvent () {
    const buttonDiv = document.getElementById('button-div')
    this.isOnBottom = buttonDiv.getBoundingClientRect().top < window.innerHeight
  }

  scrollToBottom () {
    this.content.scrollToBottom(500)
  }

  async loadData (e: any): Promise<void> {
    await this.getLogs()
    e.target.complete()
  }
}
