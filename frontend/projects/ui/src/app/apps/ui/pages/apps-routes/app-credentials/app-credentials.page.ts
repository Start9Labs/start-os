import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ToastController } from '@ionic/angular'
import {
  ErrorToastService,
  getPkgId,
  copyToClipboard,
  pauseFor,
} from '@start9labs/shared'

@Component({
  selector: 'app-credentials',
  templateUrl: './app-credentials.page.html',
  styleUrls: ['./app-credentials.page.scss'],
})
export class AppCredentialsPage {
  readonly pkgId = getPkgId(this.route)
  credentials: Record<string, string> = {}
  unmasked: { [key: string]: boolean } = {}
  loading = true

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly toastCtrl: ToastController,
  ) {}

  async ngOnInit() {
    await this.getCredentials()
  }

  async refresh() {
    await this.getCredentials()
  }

  async copy(text: string): Promise<void> {
    const success = await copyToClipboard(text)
    const message = success
      ? 'Copied. Clearing clipboard in 20 seconds'
      : 'Failed to copy.'

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 2000,
    })
    await toast.present()
  }

  toggleMask(key: string) {
    this.unmasked[key] = !this.unmasked[key]
  }

  private async getCredentials(): Promise<void> {
    this.loading = true
    try {
      this.credentials = await this.embassyApi.getPackageCredentials({
        id: this.pkgId,
      })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
