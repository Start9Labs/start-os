import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
  signal,
} from '@angular/core'
import { ErrorService, i18nPipe, LoadingService } from '@start9labs/shared'
import { TuiObfuscatePipe } from '@taiga-ui/cdk'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { FormsModule } from '@angular/forms'
import { TuiBadge, TuiSwitch } from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GatewayAddress, MappedServiceInterface } from '../interface.service'
import { AddressActionsComponent } from './actions.component'
import { DomainHealthService } from './domain-health.service'

@Component({
  selector: 'tr[address]',
  host: {
    '[class._disabled]': '!address().enabled',
  },
  template: `
    @if (address(); as address) {
      <td>
        <input
          type="checkbox"
          tuiSwitch
          size="s"
          [showIcons]="false"
          [disabled]="
            toggling() || address.hostnameInfo.metadata.kind === 'mdns'
          "
          [ngModel]="address.enabled"
          (ngModelChange)="onToggleEnabled()"
        />
      </td>
      <td class="access">
        <tui-icon
          [icon]="address.access === 'public' ? '@tui.globe' : '@tui.house'"
        />
        <span>
          {{ (address.access === 'public' ? 'Public' : 'Local') | i18n }}
        </span>
      </td>
      <td class="type">
        <tui-badge
          size="s"
          [appearance]="typeAppearance(address.hostnameInfo.metadata.kind)"
        >
          {{ address.type }}
        </tui-badge>
      </td>
      <td>
        <div class="cert">
          @if (address.certificate === 'Root CA') {
            <img src="assets/icons/favicon.svg" alt="" class="cert-icon" />
          } @else if (address.certificate.startsWith("Let's Encrypt")) {
            <img src="assets/icons/letsencrypt.svg" alt="" class="cert-icon" />
          } @else if (
            address.certificate !== '-' && address.certificate !== 'Self signed'
          ) {
            <tui-icon icon="@tui.shield" class="cert-icon" />
          }
          {{ address.certificate }}
        </div>
      </td>
      <td>
        <div class="url">
          @if (address.masked && currentlyMasked()) {
            <span>{{ address.url | tuiObfuscate: 'mask' }}</span>
          } @else {
            <span [title]="address.url">
              @if (urlParts(); as parts) {
                {{ parts.prefix }}
                <b>{{ parts.hostname }}</b>
                {{ parts.suffix }}
              } @else {
                {{ address.url }}
              }
            </span>
          }
          @if (address.masked) {
            <button
              tuiIconButton
              appearance="flat-grayscale"
              size="xs"
              [iconStart]="currentlyMasked() ? '@tui.eye' : '@tui.eye-off'"
              (click)="currentlyMasked.set(!currentlyMasked())"
            >
              {{ (currentlyMasked() ? 'Reveal' : 'Hide') | i18n }}
            </button>
          }
        </div>
      </td>
      <td
        actions
        [address]="address"
        [packageId]="packageId()"
        [value]="value()"
        [disabled]="!isRunning()"
        [gatewayId]="gatewayId()"
        [style.width.rem]="5"
      ></td>
    }
  `,
  styles: `
    :host {
      grid-template-columns: fit-content(10rem) 1fr 2rem 2rem;
    }

    .access tui-icon {
      font-size: 1.3rem;
      margin-right: 0.7rem;
      vertical-align: middle;
    }

    .cert {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    .cert-icon {
      height: 1.25rem;
      width: 1.25rem;
      flex-shrink: 0;
    }

    tui-icon.cert-icon {
      font-size: 1.25rem;
    }

    .url {
      display: flex;
      align-items: center;
      gap: 0.25rem;

      span {
        white-space: normal;
        word-break: break-all;
        display: -webkit-box;
        -webkit-box-orient: vertical;
        -webkit-line-clamp: 1;
        overflow: hidden;
      }
    }

    :host-context(tui-root._mobile) {
      padding-inline-start: 0.75rem !important;
      row-gap: 0.25rem;

      &::before {
        content: '';
        position: absolute;
        inset-inline-start: 0;
        top: 0.25rem;
        bottom: 0.25rem;
        width: 4px;
        background: var(--tui-status-positive);
        border-radius: 2px;
      }

      &._disabled::before {
        background: var(--tui-background-neutral-1-hover);
      }

      td {
        width: auto !important;
        align-content: center;
      }

      td:first-child {
        display: none;
      }

      .access {
        padding-right: 0;
        font: var(--tui-font-text-m);
        font-weight: bold;

        tui-icon {
          display: none;
        }
      }

      .type {
        font: var(--tui-font-text-m);
        font-weight: bold;
        color: var(--tui-text-primary);
        padding-inline-end: 0.5rem;
      }

      td:nth-child(4) {
        grid-area: 2 / 1 / 2 / 3;

        .cert-icon {
          display: none;
        }
      }

      td:nth-child(5) {
        grid-area: 3 / 1 / 3 / 3;
      }

      td:last-child {
        grid-area: 1 / 3 / 4 / 5;
        align-self: center;
        justify-self: end;
      }
    }
  `,
  imports: [
    i18nPipe,
    AddressActionsComponent,
    TuiBadge,
    TuiButton,
    TuiIcon,
    TuiObfuscatePipe,
    TuiSwitch,
    FormsModule,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InterfaceAddressItemComponent {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly domainHealth = inject(DomainHealthService)

  readonly address = input.required<GatewayAddress>()
  readonly packageId = input('')
  readonly value = input<MappedServiceInterface | undefined>()
  readonly isRunning = input.required<boolean>()
  readonly gatewayId = input('')

  readonly toggling = signal(false)
  readonly currentlyMasked = signal(true)
  readonly recipe = computed(() =>
    this.address()?.masked && this.currentlyMasked() ? 'mask' : 'none',
  )

  readonly urlParts = computed(() => {
    const { url, hostnameInfo } = this.address()
    const idx = url.indexOf(hostnameInfo.hostname)
    if (idx === -1) return null
    return {
      prefix: url.slice(0, idx),
      hostname: hostnameInfo.hostname,
      suffix: url.slice(idx + hostnameInfo.hostname.length),
    }
  })

  typeAppearance(kind: string): string {
    switch (kind) {
      case 'public-domain':
      case 'private-domain':
        return 'info'
      case 'mdns':
        return 'positive'
      case 'ipv4':
        return 'warning'
      case 'ipv6':
        return 'neutral'
      default:
        return 'neutral'
    }
  }

  async onToggleEnabled() {
    const addr = this.address()
    const iface = this.value()
    if (!iface) return

    this.toggling.set(true)
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

      if (enabled) {
        const kind = addr.hostnameInfo.metadata.kind
        if (kind === 'public-domain' && addr.hostnameInfo.port !== null) {
          await this.domainHealth.checkPublicDomain(
            addr.hostnameInfo.hostname,
            this.gatewayId(),
            addr.hostnameInfo.port,
          )
        } else if (kind === 'private-domain') {
          await this.domainHealth.checkPrivateDomain(this.gatewayId())
        } else if (
          kind === 'ipv4' &&
          addr.access === 'public' &&
          addr.hostnameInfo.port !== null
        ) {
          await this.domainHealth.checkPortForward(
            this.gatewayId(),
            addr.hostnameInfo.port,
          )
        }
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
      this.toggling.set(false)
    }
  }
}
