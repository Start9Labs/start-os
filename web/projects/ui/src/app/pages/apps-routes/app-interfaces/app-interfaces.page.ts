import { Component, Inject, Input } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { copyToClipboard, getPkgId } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { combineLatest, map } from 'rxjs'
import { HostInfo, ServiceInterface } from '@start9labs/start-sdk/mjs/lib/types'

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

  readonly serviceInterfaces$ = combineLatest([
    this.patch.watch$(
      'package-data',
      this.pkgId,
      'installed',
      'service-interfaces',
    ),
    this.patch.watch$('package-data', this.pkgId, 'installed', 'hosts'),
  ]).pipe(
    map(([interfaces, hosts]) => {
      const sorted = Object.values(interfaces)
        .sort(iface =>
          iface.name.toLowerCase() > iface.name.toLowerCase() ? -1 : 1,
        )
        .map(iface => ({
          ...iface,
          addresses: getAddresses(iface, hosts[iface.addressInfo.hostId]),
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
  serviceInterface: ServiceInterface,
  host: HostInfo,
): MappedAddress[] {
  const addressInfo = serviceInterface.addressInfo
  const username = addressInfo.username ? addressInfo.username + '@' : ''
  const suffix = addressInfo.suffix ? '/' + addressInfo.suffix : ''

  const hostnames =
    host.kind === 'multi'
      ? host.hostnames
      : host.hostname
      ? [host.hostname]
      : []

  return hostnames
    .map(h => {
      const addresses: MappedAddress[] = []

      let name = ''
      let hostname = ''

      if (h.kind === 'onion') {
        name = 'Tor'
        hostname = h.hostname.value
      } else {
        name = h.hostname.kind
        hostname =
          h.hostname.kind === 'domain'
            ? `${h.hostname.subdomain}.${h.hostname.domain}`
            : h.hostname.value
      }

      if (h.hostname.sslPort) {
        const port = h.hostname.sslPort === 443 ? '' : `:${h.hostname.sslPort}`
        const scheme = addressInfo.bindOptions.addSsl?.scheme
          ? `${addressInfo.bindOptions.addSsl.scheme}://`
          : ''

        addresses.push({
          name,
          url: `${scheme}${username}${hostname}${port}${suffix}`,
        })
      }

      if (h.hostname.port) {
        const port = h.hostname.port === 80 ? '' : `:${h.hostname.port}`
        const scheme = addressInfo.bindOptions.scheme
          ? `${addressInfo.bindOptions.scheme}://`
          : ''

        addresses.push({
          name,
          url: `${scheme}${username}${hostname}${port}${suffix}`,
        })
      }

      return addresses
    })
    .flat()
}
