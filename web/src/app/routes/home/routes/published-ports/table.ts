import {
  ChangeDetectionStrategy,
  Component,
  input,
  output,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable, TuiTableDirective } from '@taiga-ui/addon-table'
import { TuiComparator } from '@taiga-ui/addon-table/types'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiHint,
  TuiLink,
} from '@taiga-ui/core'
import { TuiSorterPipe } from 'src/app/pipes/sorter.pipe'
import { Copy } from 'src/app/routes/home/components/copy'
import { Placeholder } from 'src/app/routes/home/components/placeholder'
import { injectFormService } from 'src/app/services/form.service'
import { PublishedPortDisplay } from './types'

const PROTOCOL_LABELS = {
  tcp: 'TCP',
  udp: 'UDP',
  'tcp+udp': 'TCP + UDP',
} as const

@Component({
  selector: '[publishedPorts]',
  template: `
    <thead tuiThead>
      <tr>
        <th tuiTh [style.width.rem]="3" [sorter]="'status' | tuiSorter"></th>
        <th tuiTh [style.min-width.rem]="10.5" [sorter]="'label' | tuiSorter">
          Label
        </th>
        <th
          tuiTh
          [style.min-width.rem]="10"
          [sorter]="'deviceName' | tuiSorter"
        >
          Device
        </th>
        <th tuiTh [style.min-width.rem]="6" [sorter]="'ports' | tuiSorter">
          Port
        </th>
        <th tuiTh [style.min-width.rem]="8" [sorter]="'protocol' | tuiSorter">
          Protocol
        </th>
        <th tuiTh [style.min-width.rem]="9" [sorter]="ipSorter">IP Version</th>
        <th tuiTh [style.min-width.rem]="6" [sorter]="'source' | tuiSorter">
          Source
        </th>
        <th tuiTh [style.min-width.rem]="16">Endpoints</th>
        <th tuiTh [style.width.rem]="3"></th>
      </tr>
    </thead>
    <tbody>
      @for (item of publishedPorts() | tuiTableSort; track item.id) {
        <tr>
          <td tuiTd>
            <span
              class="status-icon"
              [tuiHint]="statusHint"
              tuiHintDirection="end"
            >
              {{ statusIcon(item) }}
            </span>
            <ng-template #statusHint>
              <span class="flex">
                <strong>{{ item.status }}</strong>
                @if (item.statusReason) {
                  <span>{{ item.statusReason }}</span>
                }
              </span>
            </ng-template>
          </td>
          <td tuiTd>
            <strong>{{ item.label }}</strong>
          </td>
          <td tuiTd>
            <a
              tuiLink
              [routerLink]="['/devices', item.deviceMac]"
              [state]="{ returnUrl: '/published-ports' }"
            >
              {{ item.deviceName }}
            </a>
          </td>
          <td tuiTd>{{ item.ports }}</td>
          <td tuiTd>{{ protocolLabel(item.protocol) }}</td>
          <td tuiTd>{{ ipVersionLabel(item) }}</td>
          <td tuiTd>{{ item.source === 'any' ? 'Any' : item.source }}</td>
          <td tuiTd>
            <div class="flex">
              @if (getEndpointIpv4(item); as endpoint) {
                <button tuiLink appCopy>{{ endpoint }}</button>
              }
              @if (getEndpointIpv6(item); as endpoint) {
                <button tuiLink appCopy>{{ endpoint }}</button>
              }
              @if (!getEndpointIpv4(item) && !getEndpointIpv6(item)) {
                <span class="g-secondary">—</span>
              }
            </div>
          </td>
          <td tuiTd>
            <button
              tuiIconButton
              size="xs"
              iconStart="@tui.ellipsis-vertical"
              appearance="icon"
              tuiDropdownAuto
              tuiDropdown
            >
              Actions
              <tui-data-list
                *tuiDropdown="let close"
                size="s"
                (click)="close()"
              >
                <button
                  tuiOption
                  [iconStart]="
                    item.enabled ? '@tui.circle-pause' : '@tui.circle-play'
                  "
                  (click)="toggleEnabled(item)"
                >
                  {{ item.enabled ? 'Disable' : 'Enable' }}
                </button>
                <button
                  tuiOption
                  iconStart="@tui.pencil"
                  (click)="edit.emit(item)"
                >
                  Edit
                </button>
                <button
                  tuiOption
                  class="g-negative"
                  iconStart="@tui.trash"
                  (click)="delete(item.id)"
                >
                  Delete
                </button>
              </tui-data-list>
            </button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td tuiTd colspan="9">
            <app-placeholder icon="@tui.globe">
              No ports have been published
            </app-placeholder>
          </td>
        </tr>
      }
    </tbody>
  `,
  styles: `
    .status-icon {
      cursor: help;
      font-size: 0.875rem;
    }

    .flex {
      display: flex;
      flex-direction: column;
      align-items: flex-start;
      gap: 0.25rem;

      strong::first-letter {
        text-transform: uppercase;
      }
    }

    [tuiIconButton] {
      margin-block: -0.25rem;
    }
  `,
  hostDirectives: [TuiTableDirective],
  host: { class: 'g-table' },
  imports: [
    RouterLink,
    TuiTable,
    TuiButton,
    TuiDataList,
    TuiDropdown,
    TuiHint,
    TuiLink,
    TuiSorterPipe,
    Placeholder,
    Copy,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PublishedPortsTable {
  private readonly service = injectFormService<PublishedPortDisplay[]>()

  public readonly publishedPorts = input<PublishedPortDisplay[]>([])
  public readonly ipv4EndpointHost = input<string | null>(null)
  public readonly edit = output<PublishedPortDisplay>()

  protected readonly ipSorter: TuiComparator<PublishedPortDisplay> = (a, b) =>
    getIpVersion(a).localeCompare(getIpVersion(b))

  protected statusIcon(item: PublishedPortDisplay) {
    switch (item.status) {
      case 'active':
        return '🟢'
      case 'disabled':
        return '⚪'
      case 'error':
        return '🔴'
      case 'partial':
        return '🟡'
      case 'paused':
        return '🟠'
    }
  }

  protected protocolLabel(protocol: string) {
    return PROTOCOL_LABELS[protocol as keyof typeof PROTOCOL_LABELS] || protocol
  }

  protected ipVersionLabel(item: PublishedPortDisplay): string {
    const version = getIpVersion(item)

    if (version === 'both') return 'IPv4 + IPv6'

    return version === 'ipv6' ? 'IPv6' : 'IPv4'
  }

  protected getEndpointIpv4(item: PublishedPortDisplay): string | null {
    if (!item.ipv4) return null

    const host = this.ipv4EndpointHost()
    const port = item.ipv4PublicPort || item.ports

    return host ? `${host}:${port}` : null
  }

  protected getEndpointIpv6(item: PublishedPortDisplay): string | null {
    if (!item.ipv6 || !item.deviceIpv6) return null
    // Don't show non-routable addresses:
    // - fe80:: = link-local (not routable)
    // - fd = ULA (local only, requires WAN IPv6 for global address)
    // - :: = incomplete/placeholder
    return item.deviceIpv6.startsWith('fe80:') ||
      item.deviceIpv6.startsWith('fd') ||
      item.deviceIpv6.startsWith('::')
      ? null
      : `[${item.deviceIpv6}]:${item.ports}`
  }

  protected toggleEnabled(item: PublishedPortDisplay) {
    item.enabled = !item.enabled
    item.status = item.enabled ? 'active' : 'disabled'
    this.service.save([...this.publishedPorts()])
  }

  protected delete(id: string) {
    this.service.save(this.publishedPorts().filter(item => item.id !== id))
  }
}

function getIpVersion(item: PublishedPortDisplay): 'ipv4' | 'ipv6' | 'both' {
  if (item.ipv4 && item.ipv6) return 'both'

  return item.ipv6 ? 'ipv6' : 'ipv4'
}
