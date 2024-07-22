import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import {
  AlertController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import { copyToClipboard, Exver, MarkdownComponent } from '@start9labs/shared'
import { MarketplacePkg } from '../../../types'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'

@Component({
  selector: 'marketplace-additional',
  templateUrl: 'additional.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AdditionalComponent {
  @Input()
  pkg!: MarketplacePkg

  @Output()
  version = new EventEmitter<string>()

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly exver: Exver,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly toastCtrl: ToastController,
  ) {}

  async copy(address: string): Promise<void> {
    const success = await copyToClipboard(address)
    const message = success
      ? 'Copied to clipboard!'
      : 'Failed to copy to clipboard.'

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  async presentAlertVersions() {
    const versions = Object.keys(this.pkg.otherVersions).filter(
      v => this.exver.getFlavor(v) === this.pkg.flavor,
    )
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      inputs: versions
        .sort((a, b) => -1 * (this.exver.compareExver(a, b) || 0))
        .map(v => ({
          name: v, // for CSS
          type: 'radio',
          label: v, // appearance on screen
          value: v, // literal SEM version value
          checked: this.pkg.version === v,
        })),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Ok',
          handler: (version: string) => this.version.emit(version),
        },
      ],
    })

    await alert.present()
  }

  async presentModalMd(asset: 'license' | 'instructions') {
    const content = this.marketplaceService.fetchStatic$(
      this.pkg,
      asset === 'license' ? 'LICENSE.md' : 'instructions.md',
    )

    const modal = await this.modalCtrl.create({
      componentProps: { title: asset, content },
      component: MarkdownComponent,
    })

    await modal.present()
  }
}
