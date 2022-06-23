import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { ToastController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { copyToClipboard, strip } from 'src/app/util/web.util'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  readonly pkgId = getPkgId(this.route)

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
