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
  page = 1
  pkgId: string
  firstTimeLoaded = false
  needInfinite = false
  pageLength = 20

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
        limit: this.pageLength,
        page: this.page,
      })
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

      const wrapper = document.getElementById('ion-content')
      this.needInfinite = logs.length === this.pageLength
    } catch (e) {
      this.errToast.present(e)
    }
  }

  async loadData (e: any): Promise<void> {
    await this.getLogs()
    this.page++
    e.target.complete()
  }
}
