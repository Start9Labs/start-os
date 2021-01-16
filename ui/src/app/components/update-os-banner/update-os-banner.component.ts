import { Component } from '@angular/core'
import { OsUpdateService } from 'src/app/services/os-update.service'
import { Observable } from 'rxjs'
import { AlertController } from '@ionic/angular'
import { LoaderService } from 'src/app/services/loader.service'

@Component({
  selector: 'update-os-banner',
  templateUrl: './update-os-banner.component.html',
  styleUrls: ['./update-os-banner.component.scss'],
})
export class UpdateOsBannerComponent {
  updateAvailable$: Observable<undefined | string>
  constructor (
    private readonly osUpdateService: OsUpdateService,
    private readonly alertCtrl: AlertController,
    private readonly loader: LoaderService,
  ) {
    this.updateAvailable$ = this.osUpdateService.watchForUpdateAvailable()
  }

  ngOnInit () { }

  async confirmUpdate (versionLatest: string) {
    const alert = await this.alertCtrl.create({
      header: `Update EmbassyOS`,
      message: `Update EmbassyOS to version ${versionLatest}?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Update',
          handler: () => this.update(versionLatest),
        },
      ],
    })
    await alert.present()
  }

  private async update (versionLatest: string) {
    return this.loader.displayDuringP(
      this.osUpdateService.updateEmbassyOS(versionLatest),
    )
  }
}
