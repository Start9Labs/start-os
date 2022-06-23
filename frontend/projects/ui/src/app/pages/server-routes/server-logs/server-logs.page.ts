import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
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

  fetchFetchLogs() {
    return async (params: {
      before_flag?: boolean
      limit?: number
      cursor?: string
    }) => {
      return this.embassyApi.getServerLogs({
        before_flag: params.before_flag,
        cursor: params.cursor,
        limit: params.limit,
      })
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
