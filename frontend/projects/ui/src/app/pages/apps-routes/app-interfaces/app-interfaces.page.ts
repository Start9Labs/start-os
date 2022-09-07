import { Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { getPkgId, copyToClipboard } from '@start9labs/shared'
import { getUiInterfaceKey } from 'src/app/services/config.service'
import {
  DataModel,
  InstalledPackageDataEntry,
  InterfaceDef,
} from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { getPackage } from '../../../util/get-package-data'

interface LocalInterface {
  def: InterfaceDef
  addresses: InstalledPackageDataEntry['interface-addresses'][string]
}

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesPage {
  ui?: LocalInterface
  other: LocalInterface[] = []
  readonly pkgId = getPkgId(this.route)

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async ngOnInit() {
    const pkg = await getPackage(this.patch, this.pkgId)
    const interfaces = pkg.manifest.interfaces
    const uiKey = getUiInterfaceKey(interfaces)

    if (!pkg.installed) return

    const addressesMap = pkg.installed['interface-addresses']

    if (uiKey) {
      const uiAddresses = addressesMap[uiKey]
      this.ui = {
        def: interfaces[uiKey],
        addresses: {
          'lan-address': uiAddresses['lan-address']
            ? 'https://' + uiAddresses['lan-address']
            : '',
          'tor-address': uiAddresses['tor-address']
            ? 'http://' + uiAddresses['tor-address']
            : '',
        },
      }
    }

    this.other = Object.keys(interfaces)
      .filter(key => key !== uiKey)
      .map(key => {
        const addresses = addressesMap[key]
        return {
          def: interfaces[key],
          addresses: {
            'lan-address': addresses['lan-address']
              ? 'https://' + addresses['lan-address']
              : '',
            'tor-address': addresses['tor-address']
              ? 'http://' + addresses['tor-address']
              : '',
          },
        }
      })
  }
}

@Component({
  selector: 'app-interfaces-item',
  templateUrl: './app-interfaces-item.component.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesItemComponent {
  @Input()
  interface!: LocalInterface

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
