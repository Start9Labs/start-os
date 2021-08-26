import { Component, Input, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { Log } from 'src/app/services/api/api.types'

@Component({
  selector: 'logs',
  templateUrl: './logs.page.html',
  styleUrls: ['./logs.page.scss'],
})
export class LogsPage {
  @ViewChild(IonContent) private content: IonContent
  @Input() fetchLogs: (params: { before?: string, after?: string, limit: number }) => Promise<Log[]>
  loading = true
  loadingMore = false
  logs: string
  needInfinite = true
  before: string
  after: string
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
      const logs = await this.fetchLogs({
        before: isBefore ? this.before : undefined,
        after: !isBefore ? this.after : undefined,
        limit: this.limit,
      })
      this.before = logs[0]?.timestamp
      this.after = logs[logs.length - 1]?.timestamp
      this.loading = false

      return logs
    } catch (e) {
      this.errToast.present(e)
    }
  }

  async getLogs () {
    try {
      // get logs
      const logs = await this.fetch()
      this.before = logs[0].timestamp

      const container = document.getElementById('container')
      const beforeContainerHeight = container.scrollHeight
      const newLogs = document.getElementById('template').cloneNode(true) as HTMLElement
      newLogs.innerHTML = logs.map(l => `${l.timestamp} ${l.log}`).join('\n\n') + (logs.length ? '\n\n' : '')
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
      const container = document.getElementById('container')
      const newLogs = document.getElementById('template').cloneNode(true) as HTMLElement
      newLogs.innerHTML = logs.map(l => `${l.timestamp} ${l.log}`).join('\n\n') + (logs.length ? '\n\n' : '')
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
