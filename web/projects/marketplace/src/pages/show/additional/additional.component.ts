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
import {
  copyToClipboard,
  displayExver,
  Exver,
  MarkdownComponent,
} from '@start9labs/shared'
import { MarketplacePkg } from '../../../types'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'
import { AbstractPkgFlavorService } from '../../../services/pkg-flavor.service'
import { ActivatedRoute } from '@angular/router'
import { ExtendedVersion } from '@start9labs/start-sdk'

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

  readonly flavorStatus$ = this.pkgFlavorService.getFlavorStatus$()

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly exver: Exver,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly pkgFlavorService: AbstractPkgFlavorService,
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

  async presentAlertVersions(flavorActive: boolean) {
    const versions = this.pkg.flavorVersion
      ? Object.keys(this.pkg.otherVersions).filter(
          v => !!ExtendedVersion.parse(v).flavor === flavorActive,
        )
      : Object.keys(this.pkg.otherVersions)
    const alert = await this.alertCtrl.create({
      header: 'Versions',
      inputs: versions
        .sort((a, b) => -1 * (this.exver.compareExver(a, b) || 0))
        .map(v => ({
          name: v, // for CSS
          type: 'radio',
          label: displayExver(v), // appearance on screen
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

  async presentModalMd(title: string) {
    const content = this.marketplaceService.fetchStatic$(this.pkg, title)

    const modal = await this.modalCtrl.create({
      componentProps: { title, content },
      component: MarkdownComponent,
    })

    await modal.present()
  }
}
