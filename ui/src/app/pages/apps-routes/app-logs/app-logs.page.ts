import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { IonContent } from '@ionic/angular'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  @ViewChild(IonContent, { static: false }) private content: IonContent
  pkgId: string
  firstTimeLoaded = false
  needInfinite = true
  before: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.getLogs()
  }

  async getLogs () {
    try {
      // get logs
      const logs = await this.embassyApi.getPackageLogs({
        id: this.pkgId,
        before: this.before,
      })
      this.firstTimeLoaded = true

      if (!logs.length) {
        this.needInfinite = false
        return
      }

      this.before = logs[0].timestamp

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
