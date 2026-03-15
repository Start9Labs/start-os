import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  signal,
} from '@angular/core'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import {
  CopyService,
  DialogService,
  ErrorService,
  i18nPipe,
} from '@start9labs/shared'
import {
  TuiButton,
  tuiButtonOptionsProvider,
  TuiDataList,
  TuiDropdown,
  TuiInput,
} from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { QRModal } from 'src/app/routes/portal/modals/qr.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GatewayAddress, MappedServiceInterface } from '../interface.service'
import { DomainHealthService } from './domain-health.service'

@Component({
  selector: 'td[actions]',
  template: `
    <div class="desktop">
      @if (address().deletable) {
        <button
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.trash"
          (click)="deleteDomain()"
        >
          {{ 'Delete' | i18n }}
        </button>
      }
      @if (address().hostnameInfo.metadata.kind === 'public-domain') {
        <button
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.settings"
          (click)="showDnsValidation()"
        >
          {{ 'Address Requirements' | i18n }}
        </button>
      }
      @if (address().hostnameInfo.metadata.kind === 'private-domain') {
        <button
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.settings"
          (click)="showPrivateDnsValidation()"
        >
          {{ 'Address Requirements' | i18n }}
        </button>
      }
      @if (
        address().hostnameInfo.metadata.kind === 'ipv4' &&
        address().access === 'public' &&
        address().hostnameInfo.port !== null
      ) {
        <button
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.settings"
          (click)="showPortForwardValidation()"
        >
          {{ 'Address Requirements' | i18n }}
        </button>
      }
      @if (address().ui) {
        <a
          tuiIconButton
          appearance="flat-grayscale"
          iconStart="@tui.external-link"
          target="_blank"
          rel="noreferrer"
          [attr.href]="address().enabled ? address().url : null"
          [class.disabled]="!address().enabled"
        >
          {{ 'Open UI' | i18n }}
        </a>
      }
      <button
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.qr-code"
        (click)="showQR()"
      >
        {{ 'Show QR' | i18n }}
      </button>
      <button
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.copy"
        (click)="copyService.copy(address().url)"
      >
        {{ 'Copy URL' | i18n }}
      </button>
    </div>
    <div class="mobile">
      <button
        tuiDropdown
        tuiIconButton
        appearance="flat-grayscale"
        iconStart="@tui.ellipsis-vertical"
        [tuiAppearanceState]="open() ? 'hover' : null"
        [(tuiDropdownOpen)]="open"
      >
        {{ 'Actions' | i18n }}
        <tui-data-list *tuiDropdown (click)="open.set(false)">
          @if (address().ui) {
            <a
              tuiOption
              iconStart="@tui.external-link"
              target="_blank"
              rel="noreferrer"
              [attr.href]="address().enabled ? address().url : null"
              [class.disabled]="!address().enabled"
            >
              {{ 'Open UI' | i18n }}
            </a>
          }
          <button
            tuiOption
            [iconStart]="
              address().enabled ? '@tui.toggle-right' : '@tui.toggle-left'
            "
            (click)="toggleEnabled()"
          >
            {{ (address().enabled ? 'Disable' : 'Enable') | i18n }}
          </button>
          <button tuiOption iconStart="@tui.qr-code" (click)="showQR()">
            {{ 'Show QR' | i18n }}
          </button>
          <button
            tuiOption
            iconStart="@tui.copy"
            (click)="copyService.copy(address().url)"
          >
            {{ 'Copy URL' | i18n }}
          </button>
          @if (address().hostnameInfo.metadata.kind === 'public-domain') {
            <button
              tuiOption
              iconStart="@tui.settings"
              (click)="showDnsValidation()"
            >
              {{ 'Address Requirements' | i18n }}
            </button>
          }
          @if (address().hostnameInfo.metadata.kind === 'private-domain') {
            <button
              tuiOption
              iconStart="@tui.settings"
              (click)="showPrivateDnsValidation()"
            >
              {{ 'Address Requirements' | i18n }}
            </button>
          }
          @if (
            address().hostnameInfo.metadata.kind === 'ipv4' &&
            address().hostnameInfo.port !== null
          ) {
            <button
              tuiOption
              iconStart="@tui.settings"
              (click)="showPortForwardValidation()"
            >
              {{ 'Address Requirements' | i18n }}
            </button>
          }
          @if (address().deletable) {
            <button tuiOption iconStart="@tui.trash" (click)="deleteDomain()">
              {{ 'Delete' | i18n }}
            </button>
          }
        </tui-data-list>
      </button>
    </div>
  `,
  styles: `
    :host {
      text-align: right;
      grid-area: 1/4/4/4;
      width: fit-content;
      place-content: center;
      white-space: nowrap;
    }

    .disabled {
      pointer-events: none;
      opacity: var(--tui-disabled-opacity);
    }

    .mobile {
      display: none;
    }

    :host-context(tui-root._mobile) {
      .desktop {
        display: none;
      }

      .mobile {
        display: block;
      }
    }
  `,
  imports: [TuiButton, TuiDropdown, TuiDataList, i18nPipe, TuiInput],
  providers: [tuiButtonOptionsProvider({ appearance: 'icon' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AddressActionsComponent {
  private readonly isMobile = inject(WA_IS_MOBILE)
  private readonly dialog = inject(DialogService)
  private readonly api = inject(ApiService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly domainHealth = inject(DomainHealthService)
  readonly copyService = inject(CopyService)
  readonly open = signal(false)

  readonly address = input.required<GatewayAddress>()
  readonly packageId = input('')
  readonly value = input<MappedServiceInterface | undefined>()
  readonly disabled = input.required<boolean>()
  readonly gatewayId = input('')

  showQR() {
    this.dialog
      .openComponent(new PolymorpheusComponent(QRModal), {
        size: 's',
        closable: this.isMobile,
        data: this.address().url,
      })
      .subscribe()
  }

  async toggleEnabled() {
    const addr = this.address()
    const iface = this.value()
    if (!iface) return

    const enabled = !addr.enabled
    const addressJson = JSON.stringify(addr.hostnameInfo)
    const loader = this.loader.open('Saving').subscribe()

    try {
      if (this.packageId()) {
        await this.api.pkgBindingSetAddressEnabled({
          internalPort: iface.addressInfo.internalPort,
          address: addressJson,
          enabled,
          package: this.packageId(),
          host: iface.addressInfo.hostId,
        })
      } else {
        await this.api.serverBindingSetAddressEnabled({
          internalPort: 80,
          address: addressJson,
          enabled,
        })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  showDnsValidation() {
    const port = this.address().hostnameInfo.port
    if (port === null) return
    this.domainHealth.showPublicDomainSetup(
      this.address().hostnameInfo.hostname,
      this.gatewayId(),
      port,
    )
  }

  showPrivateDnsValidation() {
    this.domainHealth.showPrivateDomainSetup(this.gatewayId())
  }

  showPortForwardValidation() {
    const port = this.address().hostnameInfo.port
    if (port === null) return
    this.domainHealth.showPortForwardSetup(this.gatewayId(), port)
  }

  async deleteDomain() {
    const addr = this.address()
    const iface = this.value()
    if (!iface) return

    const confirmed = await this.dialog
      .openConfirm({ label: 'Are you sure?', size: 's' })
      .toPromise()

    if (!confirmed) return

    const loader = this.loader.open('Removing').subscribe()

    try {
      const host = addr.hostnameInfo.hostname

      if (addr.hostnameInfo.metadata.kind === 'public-domain') {
        if (this.packageId()) {
          await this.api.pkgRemovePublicDomain({
            fqdn: host,
            package: this.packageId(),
            host: iface.addressInfo.hostId,
          })
        } else {
          await this.api.osUiRemovePublicDomain({ fqdn: host })
        }
      } else if (addr.hostnameInfo.metadata.kind === 'private-domain') {
        if (this.packageId()) {
          await this.api.pkgRemovePrivateDomain({
            fqdn: host,
            package: this.packageId(),
            host: iface.addressInfo.hostId,
          })
        } else {
          await this.api.osUiRemovePrivateDomain({ fqdn: host })
        }
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
