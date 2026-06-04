import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
  signal,
} from '@angular/core'
import { DialogService, ErrorService, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiCell, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiNotificationMiddleService, TuiSegmented } from '@taiga-ui/kit'
import { PORT_RANGE_FORWARD } from 'src/app/routes/portal/components/port-ranges/port-range-forward.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GatewayService } from 'src/app/services/gateway.service'

export type MappedPortRange = {
  hostId: string
  internalStartPort: number
  externalStartPort: number
  numberOfPorts: number
  gatewayAccess: { [id: string]: T.RangeGatewayAccess }
}

type AccessOption = {
  access: T.RangeGatewayAccess
  icon: string
  label: string
}

@Component({
  selector: 'service-port-range',
  template: `
    <header>
      {{ 'Range' | i18n }}:
      {{ format() }}
    </header>
    @for (gw of gateways(); track gw.id) {
      <div tuiCell>
        @switch (gw.ipInfo.deviceType) {
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
        <span tuiTitle>
          <b>{{ gw.name }}</b>
        </span>
        <tui-segmented size="s" [activeItemIndex]="indexFor(gw.id)">
          @for (opt of options; track opt.access) {
            <button
              type="button"
              [attr.data-access]="opt.access"
              [disabled]="
                !!busy()[gw.id] || (opt.access === 'lan-wan' && !gw.wanIp)
              "
              [title]="
                opt.access === 'lan-wan' && !gw.wanIp
                  ? ('No WAN IP' | i18n)
                  : opt.label
              "
              (click)="set(gw.id, opt.access)"
            >
              <tui-icon [icon]="opt.icon" />
              <span class="label">{{ opt.label }}</span>
            </button>
          }
        </tui-segmented>
      </div>
    } @empty {
      <div tuiCell>
        <span tuiTitle>{{ 'No gateways' | i18n }}</span>
      </div>
    }
  `,
  styles: `
    :host {
      max-width: 48rem;
    }

    [data-access='disabled'] tui-icon {
      color: var(--tui-status-negative);
    }

    [data-access='lan'] tui-icon {
      color: var(--tui-status-warning);
    }

    [data-access='lan-wan'] tui-icon {
      color: var(--tui-status-positive);
    }

    :host-context(tui-root._mobile) .label {
      display: none;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiCell, TuiIcon, TuiTitle, TuiSegmented, i18nPipe],
})
export class PortRangeComponent {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly gatewayService = inject(GatewayService)
  private readonly dialog = inject(DialogService)
  private readonly i18n = inject(i18nPipe)

  readonly packageId = input.required<string>()
  readonly value = input.required<MappedPortRange>()

  readonly busy = signal<Record<string, boolean>>({})

  // Disabled → off (red), LAN → LAN-only (amber), LAN+WAN → public (green).
  protected readonly options: readonly AccessOption[] = [
    {
      access: 'disabled',
      icon: '@tui.x',
      label: this.i18n.transform('Disabled'),
    },
    {
      access: 'lan',
      icon: '@tui.house',
      label: this.i18n.transform('LAN'),
    },
    {
      access: 'lan-wan',
      icon: '@tui.globe',
      label: this.i18n.transform('LAN+WAN'),
    },
  ]

  // Shown in the card header and the forwarding dialog (copyable), e.g.
  // "49152-49251" — a plain hyphen so it pastes cleanly into router configs.
  readonly format = computed(() => {
    const { externalStartPort: start, numberOfPorts: count } = this.value()
    return count > 1 ? `${start}-${start + count - 1}` : `${start}`
  })

  // Inbound gateways only — outbound-only gateways can't receive forwards.
  readonly gateways = computed(() =>
    (this.gatewayService.gateways() ?? []).filter(
      gw => gw.type !== 'outbound-only',
    ),
  )

  indexFor(gatewayId: string): number {
    switch (this.accessFor(gatewayId)) {
      case 'disabled':
        return 0
      case 'lan':
        return 1
      default:
        return 2
    }
  }

  async set(gatewayId: string, access: T.RangeGatewayAccess) {
    if (this.accessFor(gatewayId) === access) return

    this.busy.update(b => ({ ...b, [gatewayId]: true }))
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.pkgBindingSetRangeAccess({
        package: this.packageId(),
        host: this.value().hostId,
        internalStartPort: this.value().internalStartPort,
        gateway: gatewayId,
        access,
      })

      // LAN+WAN exposes the range to the WAN, which requires a router
      // port-forward. A contiguous UDP range can't be reliably reachability-
      // tested, so instead of probing we always show the operator exactly what
      // to forward on this gateway (TCP + UDP) for them to confirm.
      if (access === 'lan-wan') {
        this.dialog
          .openComponent(PORT_RANGE_FORWARD, {
            label: 'Port Forwarding',
            size: 's',
            data: {
              gatewayName:
                this.gateways().find(gw => gw.id === gatewayId)?.name ??
                gatewayId,
              range: this.format(),
            },
          })
          .subscribe()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
      this.busy.update(b => ({ ...b, [gatewayId]: false }))
    }
  }

  private accessFor(gatewayId: string): T.RangeGatewayAccess {
    return this.value().gatewayAccess[gatewayId] ?? 'lan'
  }
}
