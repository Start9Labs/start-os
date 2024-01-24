import { Component, Inject, Input } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { copyToClipboard, getPkgId } from '@start9labs/shared'
import {
  DataModel,
  InstalledPackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { Observable, map } from 'rxjs'

type MappedIFace = {
  addresses: MappedAddress[]
  name: string
  description: string
  type: 'ui' | 'p2p' | 'api'
}

type MappedAddress =
  InstalledPackageDataEntry['network-interfaces']['']['addresses']['']

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesPage {
  readonly pkgId = getPkgId(this.route)
  readonly networkInterfaces$ = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'network-interfaces')
    .pipe(
      map(obj => {
        const sorted = Object.values(obj)
          .sort(val =>
            val.name.toLowerCase() > val.name.toLowerCase() ? -1 : 1,
          )
          .map(val => {
            let addresses: MappedAddress[] = []

            addresses.push(val.addresses['local'])
            addresses.push(val.addresses['tor'])

            addresses = addresses.concat(
              Object.keys(val.addresses)
                .filter(key => !['local', 'tor'].includes(key))
                .sort()
                .map(key => val.addresses[key]),
            )

            return {
              ...val,
              addresses,
            }
          })

        return {
          ui: sorted.filter(val => val.type === 'ui'),
          api: sorted.filter(val => val.type === 'api'),
          p2p: sorted.filter(val => val.type === 'p2p'),
        }
      }),
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
  @Input() iFace!: MappedIFace

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  launch(url: string): void {
    this.windowRef.open(url, '_blank', 'noreferrer')
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
