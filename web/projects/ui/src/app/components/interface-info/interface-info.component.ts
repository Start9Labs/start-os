import { Component, Inject, Input } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import {
  AlertController,
  ModalController,
  ToastController,
} from '@ionic/angular'
import {
  copyToClipboard,
  ErrorService,
  LoadingService,
} from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { firstValueFrom } from 'rxjs'
import { ISB, T, utils } from '@start9labs/start-sdk'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormComponent } from 'src/app/components/form.component'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { toAcmeName } from 'src/app/util/acme'
import { ConfigService } from 'src/app/services/config.service'

export type MappedInterface = T.ServiceInterface & {
  addresses: MappedAddress[]
  public: boolean
}
export type MappedAddress = {
  name: string
  url: string
  isDomain: boolean
  isOnion: boolean
  acme: string | null
}

@Component({
  selector: 'interface-info',
  templateUrl: './interface-info.component.html',
  styleUrls: ['./interface-info.component.scss'],
})
export class InterfaceInfoComponent {
  @Input() pkgId?: string
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
    private readonly config: ConfigService,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  launch(url: string): void {
    this.windowRef.open(url, '_blank', 'noreferrer')
  }

  async togglePublic() {
    const loader = this.loader
      .open(`Making ${this.iFace.public ? 'private' : 'public'}`)
      .subscribe()

    const params = {
      internalPort: this.iFace.addressInfo.internalPort,
      public: !this.iFace.public,
    }

    try {
      if (this.pkgId) {
        await this.api.pkgBindingSetPubic({
          ...params,
          host: this.iFace.addressInfo.hostId,
          package: this.pkgId,
        })
      } else {
        await this.api.serverBindingSetPubic(params)
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async presentDomainForm() {
    const acme = await firstValueFrom(this.patch.watch$('serverInfo', 'acme'))

    const spec = getDomainSpec(Object.keys(acme))

    this.formDialog.open(FormComponent, {
      label: 'Add Domain',
      data: {
        spec: await configBuilderToSpec(spec),
        buttons: [
          {
            text: 'Save',
            handler: async (val: typeof spec._TYPE) => {
              if (val.type.selection === 'standard') {
                return this.saveStandard(
                  val.type.value.domain,
                  val.type.value.acme,
                )
              } else {
                return this.saveTor(val.type.value.key)
              }
            },
          },
        ],
      },
    })
  }

  async removeStandard(url: string) {
    const loader = this.loader.open('Removing').subscribe()

    const params = {
      domain: new URL(url).hostname,
    }

    try {
      if (this.pkgId) {
        await this.api.pkgRemoveDomain({
          ...params,
          package: this.pkgId,
          host: this.iFace.addressInfo.hostId,
        })
      } else {
        await this.api.serverRemoveDomain(params)
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async removeOnion(url: string) {
    const loader = this.loader.open('Removing').subscribe()

    const params = {
      onion: new URL(url).hostname,
    }

    try {
      if (this.pkgId) {
        await this.api.pkgRemoveOnion({
          ...params,
          package: this.pkgId,
          host: this.iFace.addressInfo.hostId,
        })
      } else {
        await this.api.serverRemoveOnion(params)
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async showAcme(url: string | null): Promise<void> {
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

  private async saveStandard(domain: string, acme: string) {
    const loader = this.loader.open('Saving').subscribe()

    const params = {
      domain,
      acme: acme === 'none' ? null : acme,
      private: false,
    }

    try {
      if (this.pkgId) {
        await this.api.pkgAddDomain({
          ...params,
          package: this.pkgId,
          host: this.iFace.addressInfo.hostId,
        })
      } else {
        await this.api.serverAddDomain(params)
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async saveTor(key: string | null) {
    const loader = this.loader.open('Creating onion address').subscribe()

    try {
      let onion = key
        ? await this.api.addTorKey({ key })
        : await this.api.generateTorKey({})
      onion = `${onion}.onion`

      if (this.pkgId) {
        await this.api.pkgAddOnion({
          onion,
          package: this.pkgId,
          host: this.iFace.addressInfo.hostId,
        })
      } else {
        await this.api.serverAddOnion({ onion })
      }
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}

function getDomainSpec(acme: string[]) {
  return ISB.InputSpec.of({
    type: ISB.Value.union(
      { name: 'Type', default: 'standard' },
      ISB.Variants.of({
        standard: {
          name: 'Standard',
          spec: ISB.InputSpec.of({
            domain: ISB.Value.text({
              name: 'Domain',
              description: 'The domain or subdomain you want to use',
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
          }),
        },
        onion: {
          name: 'Onion',
          spec: ISB.InputSpec.of({
            key: ISB.Value.text({
              name: 'Private Key (optional)',
              description:
                'Optionally provide a base64-encoded ed25519 private key for generating the Tor V3 (.onion) address. If not provided, a random key will be generated and used.',
              required: false,
              default: null,
              patterns: [utils.Patterns.base64],
            }),
          }),
        },
      }),
    ),
  })
}

export function getAddresses(
  serviceInterface: T.ServiceInterface,
  host: T.Host,
  config: ConfigService,
): MappedAddress[] {
  const addressInfo = serviceInterface.addressInfo

  let hostnames = host.hostnameInfo[addressInfo.internalPort]

  hostnames = hostnames.filter(
    h =>
      config.isLocalhost() ||
      h.kind !== 'ip' ||
      h.hostname.kind !== 'ipv6' ||
      !h.hostname.value.startsWith('fe80::'),
  )
  if (config.isLocalhost()) {
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
    let isOnion = false
    let acme: string | null = null

    if (h.kind === 'onion') {
      name = `Tor`
      isOnion = true
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
        isOnion,
        acme,
      }))
    } else {
      return addresses.map(url => ({
        name,
        url,
        isDomain,
        isOnion,
        acme,
      }))
    }
  })

  return mappedAddresses.filter(
    (value, index, self) => index === self.findIndex(t => t.url === value.url),
  )
}
