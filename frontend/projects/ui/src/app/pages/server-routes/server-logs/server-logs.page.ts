import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { copyToClipboard, strip } from 'src/app/util/web.util'

@Component({
  selector: 'server-logs',
  templateUrl: './server-logs.page.html',
  styleUrls: ['./server-logs.page.scss'],
})
export class ServerLogsPage {
  constructor(
    private readonly embassyApi: ApiService,
    private readonly toastCtrl: ToastController,
  ) {}

  followLogs() {
    return async (params: RR.FollowServerLogsReq) => {
      return this.embassyApi.followServerLogs(params)
    }
  }

  fetchLogs() {
    return async (params: RR.GetServerLogsReq) => {
      return this.embassyApi.getServerLogs(params)
    }
  }

  async copy(): Promise<void> {
    const logs = document
      .getElementById('template')
      ?.cloneNode(true) as HTMLElement
    const formatted = '```' + strip(logs.innerHTML) + '```'
    const success = await copyToClipboard(formatted)
    const message = success
      ? 'Copied to clipboard!'
      : 'Failed to copy to clipboard.'

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }
}
