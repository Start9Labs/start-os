import { Component } from '@angular/core'
import { OsUpdateService } from 'src/app/services/os-update.service'
import { Observable } from 'rxjs'
import { ModalController } from '@ionic/angular'
import { WizardBaker } from '../install-wizard/prebaked-wizards'
import { wizardModal } from '../install-wizard/install-wizard.component'
import { ReqRes } from 'src/app/services/api/api.service'

@Component({
  selector: 'update-os-banner',
  templateUrl: './update-os-banner.component.html',
  styleUrls: ['./update-os-banner.component.scss'],
})
export class UpdateOsBannerComponent {
  updateAvailable$: Observable<undefined | ReqRes.GetVersionLatestRes>
  constructor (
    private readonly osUpdateService: OsUpdateService,
    private readonly modalCtrl: ModalController,
    private readonly wizardBaker: WizardBaker,
  ) {
    this.updateAvailable$ = this.osUpdateService.watchForUpdateAvailable$()
  }

  ngOnInit () { }

  async confirmUpdate (res: ReqRes.GetVersionLatestRes) {
    await wizardModal(
      this.modalCtrl,
      this.wizardBaker.updateOS({
        version: res.versionLatest,
        releaseNotes: res.releaseNotes,
      }),
    )
  }
}
