import { Component, Inject, Input } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { ActivatedRoute } from '@angular/router'
import {
  AlertController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import {
  copyToClipboard,
  ErrorService,
  getPkgId,
  LoadingService,
} from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { combineLatest, firstValueFrom, map } from 'rxjs'
import { ISB, T, utils } from '@start9labs/start-sdk'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormComponent } from 'src/app/components/form.component'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { ACME_URL, toAcmeName } from 'src/app/util/acme'

type MappedInterface = T.ServiceInterface & {
  addresses: MappedAddress[]
  public: boolean
}
type MappedAddress = {
  name: string
  url: string
  isDomain: boolean
  acme: string | null
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
          const host = hosts[iface.addressInfo.hostId]
          return {
            ...iface,
            public: host.bindings[iface.addressInfo.internalPort].net.public,
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
  @Input() pkgId!: string
  @Input() iFace!: MappedInterface

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly formDialog: FormDialogService,
    private readonly alertCtrl: AlertController,
    private readonly patch: PatchDB<DataModel>,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  launch(url: string): void {
    this.windowRef.open(url, '_blank', 'noreferrer')
  }

  async togglePublic() {
    const loader = this.loader
      .open(`Making ${this.iFace.public ? 'private' : 'public'}`)
      .subscribe()

    try {
      await this.api.bindingSetPubic({
        host: this.iFace.addressInfo.hostId,
        internalPort: this.iFace.addressInfo.internalPort,
        package: this.pkgId,
        public: !this.iFace.public,
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async presentDomainForm() {
    const acme = await firstValueFrom(this.patch.watch$('serverInfo', 'acme'))

    this.formDialog.open(FormComponent, {
      label: 'Add Domain',
      data: {
        spec: await configBuilderToSpec(getDomainSpec(Object.keys(acme))),
        buttons: [
          {
            text: 'Save',
            handler: async (val: { domain: string; acme: string }) =>
              this.saveDomain(val.domain, val.acme),
          },
        ],
      },
    })
  }

  async removeDomain(url: string) {
    const loader = this.loader.open('Removing').subscribe()

    try {
      await this.api.removeDomain({
        package: this.pkgId,
        host: this.iFace.addressInfo.hostId,
        domain: new URL(url).hostname,
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async showAcme(url: ACME_URL | string | null): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'ACME Provider',
      message: toAcmeName(url),
    })
    await alert.present()
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

  private async saveDomain(domain: string, acme: string) {
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.addDomain({
        package: this.pkgId,
        host: this.iFace.addressInfo.hostId,
        domain,
        acme: acme === 'none' ? null : acme,
        private: false,
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
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
  const mappedAddresses = hostnames.flatMap(h => {
    let name = ''
    let isDomain = false
    let acme: string | null = null

    if (h.kind === 'onion') {
      name = `Tor`
    } else {
      const hostnameKind = h.hostname.kind

      if (hostnameKind === 'domain') {
        name = 'Domain'
        isDomain = true
        acme = host.domains[h.hostname.domain]?.acme
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
        isDomain,
        acme,
      }))
    } else {
      return addresses.map(url => ({
        name,
        url,
        isDomain,
        acme,
      }))
    }
  })

  return mappedAddresses.filter(
    (value, index, self) => index === self.findIndex(t => t.url === value.url),
  )
}

function getDomainSpec(acme: string[]) {
  return ISB.InputSpec.of({
    domain: ISB.Value.text({
      name: 'Domain',
      description: 'The domain or subdomain you want to use',
      warning: null,
      placeholder: `e.g. 'mydomain.com' or 'sub.mydomain.com'`,
      required: true,
      default: null,
      patterns: [utils.Patterns.domain],
    }),
    acme: ISB.Value.select({
      name: 'ACME Provider',
      description:
        'Select which ACME provider to use for obtaining your SSL certificate. Add new ACME providers in the System tab. Optionally use your system Root CA. Note: only devices that have trusted your Root CA will be able to access the domain without security warnings.',
      values: acme.reduce(
        (obj, url) => ({
          ...obj,
          [url]: toAcmeName(url),
        }),
        { none: 'None (use system Root CA)' } as Record<string, string>,
      ),
      default: '',
    }),
  })
}
