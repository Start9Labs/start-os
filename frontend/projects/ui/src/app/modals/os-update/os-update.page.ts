import { Component, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ConfigService } from '../../services/config.service'
import { ApiService } from '../../services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'

@Component({
  selector: 'os-update',
  templateUrl: './os-update.page.html',
  styleUrls: ['./os-update.page.scss'],
})
export class OSUpdatePage {
  @Input() releaseNotes!: { [version: string]: string }

  versions: { version: string; notes: string }[] = []

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly config: ConfigService,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) {}

  ngOnInit() {
    this.versions = Object.keys(this.releaseNotes)
      .sort()
      .reverse()
      .map(version => {
        return {
          version,
          notes: this.releaseNotes[version],
        }
      })
  }

  dismiss() {
    this.modalCtrl.dismiss()
  }

  async updateEOS() {
    const loader = await this.loadingCtrl.create({
      message: 'Beginning update...',
    })
    await loader.present()

    try {
      await this.embassyApi.updateServer({
        'marketplace-url': this.config.marketplace.url,
      })
      this.dismiss()
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  asIsOrder() {
    return 0
  }
}
