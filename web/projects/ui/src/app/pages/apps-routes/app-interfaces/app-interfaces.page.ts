import { Component, Inject, Input } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { copyToClipboard, getPkgId } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { combineLatest, map } from 'rxjs'
import { T, utils } from '@start9labs/start-sdk'

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

  private readonly serviceInterfaces$ = this.patch.watch$(
    'packageData',
    this.pkgId,
    'serviceInterfaces',
  )
  private readonly hosts$ = this.patch.watch$(
    'packageData',
    this.pkgId,
    'hosts',
  )

  readonly serviceInterfacesWithHostInfo$ = combineLatest([
    this.serviceInterfaces$,
    this.hosts$,
  ]).pipe(
    map(([interfaces, hosts]) => {
      const sorted = Object.values(interfaces)
        .sort(iface =>
          iface.name.toLowerCase() > iface.name.toLowerCase() ? -1 : 1,
        )
        .map(iface => {
          return {
            ...iface,
            addresses: getAddresses(
              iface,
              hosts[iface.addressInfo.hostId] || {},
            ),
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

  let hostnames =
    host.kind === 'multi' ? host.hostnameInfo[addressInfo.internalPort] : []

  hostnames = hostnames.filter(
    h =>
      window.location.host === 'localhost' ||
      h.kind !== 'ip' ||
      h.hostname.kind !== 'ipv6' ||
      !h.hostname.value.startsWith('fe80::'),
  )
  if (window.location.host === 'localhost') {
    const local = hostnames.find(
      h => h.kind === 'ip' && h.hostname.kind === 'local',
    )
    if (local) {
      hostnames.unshift({
        kind: 'ip',
        networkInterfaceId: 'lo',
        public: false,
        hostname: {
          kind: 'local',
          port: local.hostname.port,
          sslPort: local.hostname.sslPort,
          value: 'localhost',
        },
      })
    }
  }
  const addressesWithNames = hostnames.flatMap(h => {
    let name = ''

    if (h.kind === 'onion') {
      name = `Tor`
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

    const addresses = utils.addressHostToUrl(addressInfo, h)
    if (addresses.length > 1) {
      return addresses.map(url => ({
        name: `${name} (${new URL(url).protocol
          .replace(':', '')
          .toUpperCase()})`,
        url,
      }))
    } else {
      return addresses.map(url => ({
        name,
        url,
      }))
    }
  })

  return addressesWithNames.filter(
    (value, index, self) => index === self.findIndex(t => t.url === value.url),
  )
}
