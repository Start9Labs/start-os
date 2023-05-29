import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { getPkgId, copyToClipboard } from '@start9labs/shared'
import { AddressInfo, DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { map } from 'rxjs'

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppInterfacesPage {
  readonly pkgId = getPkgId(this.route)
  readonly addressInfo$ = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'address-info')
    .pipe(
      map(addressInfo =>
        Object.values(addressInfo).sort((a, b) => a.name.localeCompare(b.name)),
      ),
    )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}

@Component({
  selector: 'app-interfaces-item',
  templateUrl: './app-interfaces-item.component.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesItemComponent {
  @Input()
  addressInfo!: AddressInfo

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
  ) {}

  launch(url: string): void {
    window.open(url, '_blank', 'noreferrer')
  }

  async showQR(text: string): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: QRComponent,
      componentProps: {
        text,
      },
      cssClass: 'qr-modal',
    })
    await modal.present()
  }

  async copy(address: string): Promise<void> {
    let message = ''
    await copyToClipboard(address || '').then(success => {
      message = success
        ? 'Copied to clipboard!'
        : 'Failed to copy to clipboard.'
    })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }
}
