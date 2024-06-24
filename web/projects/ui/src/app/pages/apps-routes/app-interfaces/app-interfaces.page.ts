import { Component, Inject, Input } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { copyToClipboard, getPkgId } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { map } from 'rxjs'
import { T } from '@start9labs/start-sdk'
import { addressHostToUrl } from '@start9labs/start-sdk/cjs/lib/util/getServiceInterface'

type MappedInterface = T.ServiceInterface & {
  addresses: MappedAddress[]
}
type MappedAddress = {
  name: string
  url: string
}

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesPage {
  readonly pkgId = getPkgId(this.route)

  readonly serviceInterfaces$ = this.patch
    .watch$('packageData', this.pkgId, 'serviceInterfaces')
    .pipe(
      map(interfaces => {
        const sorted = Object.values(interfaces)
          .sort(iface =>
            iface.name.toLowerCase() > iface.name.toLowerCase() ? -1 : 1,
          )
          .map(iface => {
            // TODO @Matt
            const host = {} as any
            return {
              ...iface,
              addresses: getAddresses(iface, host),
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
  @Input() iFace!: MappedInterface

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

function getAddresses(
  serviceInterface: T.ServiceInterface,
  host: T.Host,
): MappedAddress[] {
  const addressInfo = serviceInterface.addressInfo
  const username = addressInfo.username ? addressInfo.username + '@' : ''
  const suffix = addressInfo.suffix || ''

  const hostnames =
    host.kind === 'multi' ? host.hostnameInfo[addressInfo.internalPort] : [] // TODO: non-multi
  /* host.hostname
      ? [host.hostname]
      : [] */

  return hostnames.flatMap(h => {
    let name = ''

    if (h.kind === 'onion') {
      name = 'Tor'
    } else {
      const hostnameKind = h.hostname.kind

      if (hostnameKind === 'domain') {
        name = 'Domain'
      } else {
        name =
          hostnameKind === 'local'
            ? 'Local'
            : `${h.networkInterfaceId} (${hostnameKind})`
      }
    }

    return addressHostToUrl(addressInfo, h).map(url => ({
      name,
      url,
    }))
  })
}
