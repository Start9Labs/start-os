import { ChangeDetectionStrategy, Component } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService } from '../../services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { EOSService } from 'src/app/services/eos.service'

@Component({
  selector: 'os-update',
  templateUrl: './os-update.page.html',
  styleUrls: ['./os-update.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OSUpdatePage {
  versions: { version: string; notes: string }[] = []

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly eosService: EOSService,
  ) {}

  ngOnInit() {
    const releaseNotes = this.eosService.eos?.['release-notes']!

    this.versions = Object.keys(releaseNotes)
      .sort()
      .reverse()
      .map(version => {
        return {
          version,
          notes: releaseNotes[version],
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
      await this.embassyApi.updateServer()
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
