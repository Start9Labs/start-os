import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { ModalController, ToastController } from '@ionic/angular'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { copyToClipboard, MarkdownComponent } from '@start9labs/shared'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  InstalledState,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-additional',
  templateUrl: 'app-show-additional.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowAdditionalComponent {
  @Input()
  pkg!: PackageDataEntry<InstalledState>

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly toastCtrl: ToastController,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
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

  async presentModalLicense() {
    const { id, version } = this.pkg.stateInfo.manifest

    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'License',
        content: this.marketplaceService.fetchStatic$(
          id,
          'license',
          version,
          this.pkg.registry,
        ),
      },
      component: MarkdownComponent,
    })

    await modal.present()
  }
}
