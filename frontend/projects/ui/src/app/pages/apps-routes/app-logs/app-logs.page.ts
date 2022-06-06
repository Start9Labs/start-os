import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { ToastController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  readonly pkgId = getPkgId(this.route)
  loading = true
  needInfinite = true
  before: string

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly toastCtrl: ToastController,
  ) {}

  fetchFetchLogs() {
    return async (params: {
      before_flag?: boolean
      limit?: number
      cursor?: string
    }) => {
      return this.embassyApi.getPackageLogs({
        id: this.pkgId,
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
