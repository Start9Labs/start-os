import { Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'server-logs',
  templateUrl: './server-logs.page.html',
  styleUrls: ['./server-logs.page.scss'],
})
export class ServerLogsPage {
  pkgId: string
  loading = true
  needInfinite = true
  before: string

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
      .cloneNode(true) as HTMLElement
    const success = await this.copyToClipboard(logs.innerHTML)
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
  private async copyToClipboard(str: string): Promise<boolean> {
    const el = document.createElement('textarea')
    el.value = str
    el.setAttribute('readonly', '')
    el.style.position = 'absolute'
    el.style.left = '-9999px'
    document.body.appendChild(el)
    el.select()
    const copy = document.execCommand('copy')
    document.body.removeChild(el)
    return copy
  }
}
