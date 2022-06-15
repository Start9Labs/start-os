import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { AlertController } from '@ionic/angular'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { LocalStorageService } from 'src/app/services/local-storage.service'

@Component({
  selector: 'marketplace-show-controls',
  templateUrl: 'marketplace-show-controls.component.html',
  styleUrls: ['./marketplace-show-controls.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowControlsComponent {
  @Input()
  pkg: MarketplacePkg

  @Input()
  localPkg: PackageDataEntry | null = null

  readonly PackageState = PackageState

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly marketplaceService: AbstractMarketplaceService,
    public readonly localStorageService: LocalStorageService,
  ) {}

  get version(): string {
    return this.localPkg?.manifest.version || ''
  }

  async tryInstall() {
    const { id, title, version, alerts } = this.pkg.manifest

    if (!alerts.install) {
      this.marketplaceService.install(id, version).subscribe()
    } else {
      const alert = await this.alertCtrl.create({
        header: title,
        subHeader: version,
        message: alerts.install,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
          },
          {
            text: 'Install',
            handler: () =>
              this.marketplaceService.install(id, version).subscribe(),
          },
        ],
      })
      await alert.present()
    }
  }
}
