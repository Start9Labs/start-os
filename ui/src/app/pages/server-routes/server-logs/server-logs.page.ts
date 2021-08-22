import { Component, ViewChild } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { IonContent } from '@ionic/angular'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'server-logs',
  templateUrl: './server-logs.page.html',
  styleUrls: ['./server-logs.page.scss'],
})
export class ServerLogsPage {
  @ViewChild(IonContent, { static: false }) private content: IonContent
  loading = true
  logs: string
  needInfinite = true
  firstTimeLoaded = false
  before: string

  constructor (
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) { }

  ngOnInit () {
    this.getLogs()
  }

  async getLogs () {
    try {
      // get logs
      const logs = await this.embassyApi.getServerLogs({ before: this.before })

      if (!logs.length) {
        this.needInfinite = false
        return
      }

      this.before = logs[0].timestamp

      this.firstTimeLoaded = true

      const container = document.getElementById('container')
      const beforeContainerHeight = container.scrollHeight
      const newLogs = document.getElementById('template').cloneNode(true) as HTMLElement
      newLogs.innerHTML = logs.map(l => `${l.timestamp} ${l.log}`).join('\n\n') + '\n\n'
      container.prepend(newLogs)
      const afterContainerHeight = container.scrollHeight

      // scroll down
      scrollBy(0, afterContainerHeight - beforeContainerHeight)
      this.content.scrollToPoint(0, afterContainerHeight - beforeContainerHeight)
    } catch (e) {
      this.errToast.present(e)
    }
  }

  async loadData (e: any): Promise<void> {
    await this.getLogs()
    e.target.complete()
  }
}
