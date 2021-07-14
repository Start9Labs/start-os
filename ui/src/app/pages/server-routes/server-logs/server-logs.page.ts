import { Component, ViewChild } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
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
    private readonly apiService: ApiService,
  ) { }

  ngOnInit () {
    this.getLogs()
  }

  async getLogs () {
    this.logs = ''
    this.loading = true
    try {
      const logs = await this.apiService.getServerLogs({ })
      this.logs = logs.map(l => `${l.timestamp} ${l.log}`).join('\n\n')
      setTimeout(async () => await this.content.scrollToBottom(100), 200)
    } catch (e) {
      console.error(e)
      this.errToast.present(e.message)
    } finally {
      this.loading = false
    }
  }
}
