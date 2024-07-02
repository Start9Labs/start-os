import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { EOSService } from 'src/app/services/eos.service'
import { ApiService } from '../../services/api/embassy-api.service'

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
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly eosService: EOSService,
  ) {}
  ngOnInit() {
    const releaseNotes = this.eosService.osUpdate?.releaseNotes!

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
    const loader = this.loader.open('Beginning update...').subscribe()

    try {
      await this.embassyApi.updateServer()
      this.dismiss()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  asIsOrder() {
    return 0
  }
}
