import { inject, Injectable } from '@angular/core'
import {
  DialogService,
  ErrorService,
  i18nKey,
  LoadingService,
  i18nPipe,
} from '@start9labs/shared'
import { toSignal } from '@angular/core/rxjs-interop'
import { ISB, T, utils } from '@start9labs/start-sdk'
import { filter, map } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { toAuthorityName } from 'src/app/utils/acme'
import { InterfaceComponent } from '../interface.component'
import { DNS } from './dns.component'

export type PublicDomain = {
  fqdn: string
  gateway: GatewayWithId | null
  acme: string | null
}

export type GatewayWithId = T.NetworkInterfaceInfo & {
  id: string
  ipInfo: T.IpInfo
}

@Injectable()
export class PublicDomainService {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly dialog = inject(DialogService)
  private readonly interface = inject(InterfaceComponent)
  private readonly i18n = inject(i18nPipe)

  readonly data = toSignal(
    this.patch.watch$('serverInfo', 'network').pipe(
      map(({ gateways, acme }) => ({
        gateways: Object.entries(gateways)
          .filter(([_, g]) => g.ipInfo)
          .map(([id, g]) => ({ id, ...g })) as GatewayWithId[],
        authorities: Object.keys(acme).reduce<Record<string, string>>(
          (obj, url) => ({
            ...obj,
            [url]: toAuthorityName(url),
          }),
          { local: toAuthorityName(null) },
        ),
      })),
    ),
  )

  async add(addSsl: boolean) {
    const addSpec = ISB.InputSpec.of({
      fqdn: ISB.Value.text({
        name: this.i18n.transform('Domain'),
        description: this.i18n.transform(
          'Enter a fully qualified domain name. For example, if you control domain.com, you could enter domain.com or subdomain.domain.com or another.subdomain.domain.com.',
        ),
        required: true,
        default: null,
        patterns: [utils.Patterns.domain],
      }).map(f => f.toLocaleLowerCase()),
      ...this.gatewaySpec(),
      ...(addSsl
        ? this.authoritySpec()
        : ({} as ReturnType<typeof this.authoritySpec>)),
    })

    this.formDialog.open(FormComponent, {
      label: 'Add public domain',
      data: {
        spec: await configBuilderToSpec(addSpec),
        buttons: [
          {
            text: 'Save',
            handler: (input: typeof addSpec._TYPE) =>
              this.save(input.fqdn, input.gateway, input.authority),
          },
        ],
      },
    })
  }

  async edit(domain: PublicDomain, addSsl: boolean) {
    const editSpec = ISB.InputSpec.of({
      ...this.gatewaySpec(),
      ...(addSsl
        ? this.authoritySpec()
        : ({} as ReturnType<typeof this.authoritySpec>)),
    })

    this.formDialog.open(FormComponent, {
      label: 'Edit public domain',
      data: {
        spec: await configBuilderToSpec(editSpec),
        buttons: [
          {
            text: 'Save',
            handler: ({ gateway, authority }: typeof editSpec._TYPE) =>
              this.save(domain.fqdn, gateway, authority),
          },
        ],
        value: {
          gateway: domain.gateway!.id,
          authority: domain.acme,
        },
      },
    })
  }

  remove(fqdn: string) {
    this.dialog
      .openConfirm({ label: 'Are you sure?', size: 's' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting').subscribe()

        try {
          if (this.interface.packageId()) {
            await this.api.pkgRemovePublicDomain({
              fqdn,
              package: this.interface.packageId(),
              host: this.interface.value()?.addressInfo.hostId || '',
            })
          } else {
            await this.api.osUiRemovePublicDomain({ fqdn })
          }
          return true
        } catch (e: any) {
          this.errorService.handleError(e)
          return false
        } finally {
          loader.unsubscribe()
        }
      })
  }

  showDns(fqdn: string, gateway: GatewayWithId, message: i18nKey) {
    this.dialog
      .openComponent(DNS, {
        label: 'DNS Records',
        size: 'l',
        data: {
          fqdn,
          gateway,
          message,
        },
      })
      .subscribe()
  }

  private async save(
    fqdn: string,
    gatewayId: string,
    authority?: 'local' | string,
  ) {
    const gateway = this.data()!.gateways.find(g => g.id === gatewayId)!

    const loader = this.loader.open('Saving').subscribe()
    const params = {
      fqdn,
      gateway: gatewayId,
      acme: !authority || authority === 'local' ? null : authority,
    }
    try {
      let ip: string | null
      if (this.interface.packageId()) {
        ip = await this.api.pkgAddPublicDomain({
          ...params,
          package: this.interface.packageId(),
          host: this.interface.value()?.addressInfo.hostId || '',
        })
      } else {
        ip = await this.api.osUiAddPublicDomain(params)
      }

      const wanIp = gateway.ipInfo.wanIp

      let message = this.i18n.transform(
        'Create one of the DNS records below.',
      ) as i18nKey

      if (!ip) {
        setTimeout(
          () =>
            this.showDns(
              fqdn,
              gateway,
              `${this.i18n.transform('No DNS record detected for')} ${fqdn}. ${message}` as i18nKey,
            ),
          250,
        )
      } else if (ip !== wanIp) {
        setTimeout(
          () =>
            this.showDns(
              fqdn,
              gateway,
              `${this.i18n.transform('Invalid DNS record')}. ${fqdn} ${this.i18n.transform('resolves to')} ${ip}. ${message}` as i18nKey,
            ),
          250,
        )
      } else {
        setTimeout(
          () =>
            this.dialog
              .openAlert(
                `${fqdn} ${this.i18n.transform('resolves to')} ${wanIp}` as i18nKey,
                { label: 'DNS record detected!', appearance: 'positive' },
              )
              .subscribe(),
          250,
        )
      }

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private gatewaySpec() {
    const data = this.data()!

    const gateways = data.gateways.filter(
      ({ ipInfo: { deviceType } }) =>
        deviceType !== 'loopback' && deviceType !== 'bridge',
    )

    return {
      gateway: ISB.Value.dynamicSelect(() => ({
        name: this.i18n.transform('Gateway'),
        description: this.i18n.transform(
          'Select a gateway to use for this domain.',
        ),
        values: gateways.reduce<Record<string, string>>(
          (obj, gateway) => ({
            ...obj,
            [gateway.id]: gateway.name || gateway.ipInfo.name,
          }),
          {},
        ),
        default: '',
        disabled: gateways
          .filter(g => !g.ipInfo.wanIp || utils.CGNAT.contains(g.ipInfo.wanIp))
          .map(g => g.id),
      })),
    }
  }

  private authoritySpec() {
    const data = this.data()!

    return {
      authority: ISB.Value.select({
        name: this.i18n.transform('Certificate Authority'),
        description: this.i18n.transform(
          'Select a Certificate Authority to issue SSL/TLS certificates for this domain',
        ),
        values: data.authorities,
        default: '',
      }),
    }
  }
}
