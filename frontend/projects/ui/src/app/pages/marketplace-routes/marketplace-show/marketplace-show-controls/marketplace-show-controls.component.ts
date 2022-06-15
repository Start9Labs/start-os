import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { pauseFor } from '@start9labs/shared'

import {
  Manifest,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { wizardModal } from 'src/app/components/app-wizard/app-wizard.component'
import { WizardDefs } from 'src/app/components/app-wizard/wizard-defs'
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
    private readonly modalCtrl: ModalController,
    private readonly wizards: WizardDefs,
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
      installAlert: alerts.install || undefined,
    }

    wizardModal(
      this.modalCtrl,
      action === 'update'
        ? this.wizards.update(value)
        : this.wizards.downgrade(value),
    )
  }
}
