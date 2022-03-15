import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
  LocalPkg,
} from '@start9labs/marketplace'
import { pauseFor, PackageState } from '@start9labs/shared'

import { Manifest } from 'src/app/services/patch-db/data-model'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { LocalStorageService } from 'src/app/services/local-storage.service'

@Component({
  selector: 'marketplace-show-controls',
  templateUrl: 'marketplace-show-controls.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowControlsComponent {
  @Input()
  pkg: MarketplacePkg

  @Input()
  localPkg: LocalPkg

  readonly PackageState = PackageState

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
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

  async presentModal(action: 'update' | 'downgrade') {
    // TODO: Fix type
    const { id, title, version, dependencies, alerts } = this.pkg
      .manifest as Manifest
    const value = {
      id,
      title,
      version,
      serviceRequirements: dependencies,
      installAlert: alerts.install,
    }

    const { cancelled } = await wizardModal(
      this.modalCtrl,
      action === 'update'
        ? this.wizardBaker.update(value)
        : this.wizardBaker.downgrade(value),
    )

    if (cancelled) return

    await pauseFor(250)
    this.navCtrl.back()
  }
}
