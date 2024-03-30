import { Component, Inject, Input } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { copyToClipboard, getPkgId } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { map } from 'rxjs'
import { types as T } from '@start9labs/start-sdk'
import { ServiceInterface } from '../../../../../../../../core/startos/bindings/ServiceInterface'
import { ServiceInterfaceWithHostInfo } from '../../../../../../../../core/startos/bindings/ServiceInterfaceWithHostInfo'

type MappedInterface = ServiceInterface & {
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
          .map(iface => ({
            ...iface,
            addresses: getAddresses(iface),
          }))

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
  serviceInterface: ServiceInterfaceWithHostInfo,
): MappedAddress[] {
  const host = serviceInterface.hostInfo
  const addressInfo = serviceInterface.addressInfo
  const username = addressInfo.username ? addressInfo.username + '@' : ''
  const suffix = addressInfo.suffix || ''

  const hostnames = host.kind === 'multi' ? host.hostnames : [] // TODO: non-multi
  /* host.hostname
      ? [host.hostname]
      : [] */

  const addresses: MappedAddress[] = []

  hostnames.forEach(h => {
    let name = ''
    let hostname = ''

    if (h.kind === 'onion') {
      name = 'Tor'
      hostname = h.hostname.value
    } else {
      const hostnameKind = h.hostname.kind

      if (hostnameKind === 'domain') {
        name = 'Domain'
        hostname = `${h.hostname.subdomain}.${h.hostname.domain}`
      } else {
        name =
          hostnameKind === 'local'
            ? 'Local'
            : `${h.networkInterfaceId} (${hostnameKind})`
        hostname = h.hostname.value
      }
    }

    if (h.hostname.sslPort) {
      const port = h.hostname.sslPort === 443 ? '' : `:${h.hostname.sslPort}`
      const scheme = addressInfo.bindOptions.addSsl?.scheme
        ? `${addressInfo.bindOptions.addSsl.scheme}://`
        : ''

      addresses.push({
        name: name === 'Tor' ? 'Tor (HTTPS)' : name,
        url: `${scheme}${username}${hostname}${port}${suffix}`,
      })
    }

    if (h.hostname.port) {
      const port = h.hostname.port === 80 ? '' : `:${h.hostname.port}`
      const scheme = addressInfo.bindOptions.scheme
        ? `${addressInfo.bindOptions.scheme}://`
        : ''

      addresses.push({
        name: name === 'Tor' ? 'Tor (HTTP)' : name,
        url: `${scheme}${username}${hostname}${port}${suffix}`,
      })
    }
  })

  return addresses
}
