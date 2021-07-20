import { Component, ViewChild } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
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

  constructor (
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) { }

  ngOnInit () {
    this.getLogs()
  }

  async getLogs () {
    this.logs = ''
    this.loading = true
    try {
      const logs = await this.embassyApi.getServerLogs({ })
      this.logs = logs.map(l => `${l.timestamp} ${l.log}`).join('\n\n')
      setTimeout(async () => await this.content.scrollToBottom(100), 200)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }
}
