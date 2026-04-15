import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { DialogService, ErrorService, i18nPipe } from '@start9labs/shared'
import { ISB, utils } from '@start9labs/start-sdk'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { toAuthorityName } from 'src/app/utils/acme'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import {
  GatewayAddressGroup,
  MappedServiceInterface,
} from '../../interface.service'
import { DomainHealthService } from './domain-health.service'
import { DOMAIN_TYPE_PICKER, DomainType } from './domain-type-picker.component'
import { GatewayItemComponent } from './item.component'

@Component({
  selector: 'section[gatewayGroup]',
  template: `
    <header>
      @switch (gatewayGroup().deviceType) {
        @case ('ethernet') {
          <tui-icon icon="@tui.ethernet-port" />
        }
        @case ('wireless') {
          <tui-icon icon="@tui.wifi" />
        }
        @case ('wireguard') {
          <tui-icon icon="@tui.shield" />
        }
      }
      {{ gatewayGroup().gatewayName }}
      @if (gatewayGroup().isWireguard) {
        <button
          tuiButton
          iconStart="@tui.plus"
          [style.margin-inline-start]="'auto'"
          (click)="addPublicDomain()"
        >
          {{ 'Add Public Domain' | i18n }}
        </button>
      } @else {
        <button
          tuiButton
          iconStart="@tui.plus"
          [style.margin-inline-start]="'auto'"
          (click)="openDomainTypePicker()"
        >
          {{ 'Add Domain' | i18n }}
        </button>
      }
    </header>
    <table
      [appTable]="[
        null,
        'Access',
        'Type',
        'Certificate Authority',
        'URL',
        null,
      ]"
    >
      @for (address of gatewayGroup().addresses; track $index) {
        <tr
          [address]="address"
          [packageId]="packageId()"
          [value]="value()"
          [isRunning]="isRunning()"
          [gatewayId]="gatewayGroup().gatewayId"
        ></tr>
      } @empty {
        <tr>
          <td colspan="6">
            <app-placeholder icon="@tui.list-x">
              {{ 'No addresses' | i18n }}
            </app-placeholder>
          </td>
        </tr>
      }
    </table>
  `,
  styles: `
    header tui-icon {
      font-size: 1.25rem;
      margin-inline-end: 0.375rem;
    }

    :host ::ng-deep {
      th:first-child {
        width: 5rem;
      }
    }
  `,
  host: { class: 'g-card' },
  imports: [
    TuiButton,
    TuiIcon,
    TableComponent,
    PlaceholderComponent,
    i18nPipe,
    GatewayItemComponent,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GatewayComponent {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly formDialog = inject(FormDialogService)
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly i18n = inject(i18nPipe)
  private readonly domainHealth = inject(DomainHealthService)

  readonly gatewayGroup = input.required<GatewayAddressGroup>()
  readonly packageId = input('')
  readonly value = input<MappedServiceInterface | undefined>()
  readonly isRunning = input.required<boolean>()

  openDomainTypePicker() {
    this.dialog
      .openComponent<DomainType>(DOMAIN_TYPE_PICKER, {
        label: 'Add Domain',
        size: 'l',
      })
      .subscribe(type => {
        if (type === 'public') {
          this.addPublicDomain()
        } else if (type === 'private') {
          this.addPrivateDomain()
        }
      })
  }

  async addPrivateDomain() {
    this.formDialog.open<FormContext<{ fqdn: string }>>(FormComponent, {
      label: 'New private domain',
      size: 's',
      data: {
        spec: await configBuilderToSpec(
          ISB.InputSpec.of({
            fqdn: ISB.Value.text({
              name: this.i18n.transform('Domain'),
              description: this.i18n.transform(
                'Enter a fully qualified domain name. Since the domain is for private use, it can be any domain you want, even one you do not control.',
              ),
              required: true,
              default: null,
              patterns: [utils.Patterns.domain],
            }),
          }),
        ),
        note: this.getSharedHostNote(),
        buttons: [
          {
            text: this.i18n.transform('Save')!,
            handler: async (value: { fqdn: string }) =>
              this.savePrivateDomain(value.fqdn),
          },
        ],
      },
    })
  }

  async addPublicDomain() {
    const iface = this.value()
    if (!iface) return

    const network = await firstValueFrom(
      this.patch.watch$('serverInfo', 'network'),
    )

    const authorities = Object.keys(network.acme).reduce<
      Record<string, string>
    >(
      (obj, url) => ({
        ...obj,
        [url]: toAuthorityName(url),
      }),
      { local: toAuthorityName(null) },
    )

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
      ...(iface.addSsl
        ? {
            authority: ISB.Value.select({
              name: this.i18n.transform('Certificate Authority'),
              description: this.i18n.transform(
                'Select a Certificate Authority to issue SSL/TLS certificates for this domain',
              ),
              values: authorities,
              default: Object.keys(network.acme)[0] || 'local',
            }),
          }
        : {}),
    })

    this.formDialog.open(FormComponent, {
      label: 'Add public domain',
      size: 's',
      data: {
        spec: await configBuilderToSpec(addSpec),
        note: this.getSharedHostNote(),
        buttons: [
          {
            text: this.i18n.transform('Save')!,
            handler: (input: typeof addSpec._TYPE) =>
              this.savePublicDomain(input.fqdn, input.authority),
          },
        ],
      },
    })
  }

  private async savePrivateDomain(fqdn: string): Promise<boolean> {
    const iface = this.value()
    const gatewayId = this.gatewayGroup().gatewayId
    const loader = this.loader.open('Saving').subscribe()

    try {
      let configured: boolean
      if (this.packageId()) {
        configured = await this.api.pkgAddPrivateDomain({
          fqdn,
          gateway: gatewayId,
          package: this.packageId(),
          host: iface?.addressInfo.hostId || '',
        })
      } else {
        configured = await this.api.osUiAddPrivateDomain({
          fqdn,
          gateway: gatewayId,
        })
      }

      await this.domainHealth.checkPrivateDomain(gatewayId, configured)

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private getSharedHostNote(): string {
    const names = this.value()?.sharedHostNames
    if (!names?.length) return ''

    return `${this.i18n.transform('This domain will also apply to')} ${names.join(', ')}`
  }

  private async savePublicDomain(
    fqdn: string,
    authority?: 'local' | string,
  ): Promise<boolean> {
    const iface = this.value()
    const gatewayId = this.gatewayGroup().gatewayId
    const loader = this.loader.open('Saving').subscribe()

    const params = {
      fqdn,
      gateway: gatewayId,
      acme: !authority || authority === 'local' ? null : authority,
      internalPort: iface?.addressInfo.internalPort || 80,
    }

    try {
      let res
      if (this.packageId()) {
        res = await this.api.pkgAddPublicDomain({
          ...params,
          package: this.packageId(),
          host: iface?.addressInfo.hostId || '',
        })
      } else {
        res = await this.api.osUiAddPublicDomain(params)
      }

      await this.domainHealth.checkPublicDomain(fqdn, gatewayId, res)

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}
