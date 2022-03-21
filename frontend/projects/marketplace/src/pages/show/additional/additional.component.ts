import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { AlertController, ModalController } from '@ionic/angular'
import { displayEmver, Emver, MarkdownComponent } from '@start9labs/shared'

import { AbstractMarketplaceService } from '../../../services/marketplace.service'
import { MarketplacePkg } from '../../../types/marketplace-pkg'

@Component({
  selector: 'marketplace-additional',
  templateUrl: 'additional.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AdditionalComponent {
  @Input()
  pkg: MarketplacePkg

  @Output()
  version = new EventEmitter<string>()

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly emver: Emver,
    private readonly marketplaceService: AbstractMarketplaceService,
  ) {}

  async presentAlertVersions() {
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      inputs: this.pkg.versions
        .sort((a, b) => -1 * this.emver.compare(a, b))
        .map(v => ({
          name: v, // for CSS
          type: 'radio',
          label: displayEmver(v), // appearance on screen
          value: v, // literal SEM version value
          checked: this.pkg.manifest.version === v,
        })),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Ok',
          handler: (version: string) => this.version.emit(version),
          cssClass: 'enter-click',
        },
      ],
    })

    await alert.present()
  }

  async presentModalMd(title: string) {
    const content = this.marketplaceService.getPackageMarkdown(
      title,
      this.pkg.manifest.id,
    )

    const modal = await this.modalCtrl.create({
      componentProps: { title, content },
      component: MarkdownComponent,
    })

    await modal.present()
  }
}
