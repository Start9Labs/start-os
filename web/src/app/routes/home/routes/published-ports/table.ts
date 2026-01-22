import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
  output,
  signal,
} from '@angular/core'
import { Clipboard } from '@angular/cdk/clipboard'
import { RouterLink } from '@angular/router'
import { TuiTable, TuiTableDirective } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiHint,
  TuiIcon,
  TuiLink,
  TuiNotificationService,
} from '@taiga-ui/core'
import { Placeholder } from 'src/app/routes/home/components/placeholder'
import { injectFormService } from 'src/app/services/form.service'
import { PublishedPortDisplay, PROTOCOL_LABELS, STATUS_LABELS } from './types'

type SortColumn =
  | 'status'
  | 'name'
  | 'device'
  | 'protocol'
  | 'ipVersion'
  | 'ports'
  | 'source'
  | null
type SortDirection = 'asc' | 'desc'

@Component({
  selector: '[publishedPortsTable]',
  template: `
    <thead tuiThead>
      <tr>
        <th
          tuiTh
          [style.width.rem]="3"
          [class.active]="sortColumn() === 'status'"
          class="sortable"
          (click)="toggleSort('status')"
        >
          <tui-icon [icon]="getSortIcon('status')" />
        </th>
        <th
          tuiTh
          [style.min-width.rem]="12"
          [class.active]="sortColumn() === 'name'"
          class="sortable"
          (click)="toggleSort('name')"
        >
          Label
          <tui-icon [icon]="getSortIcon('name')" />
        </th>
        <th
          tuiTh
          [style.min-width.rem]="10"
          [class.active]="sortColumn() === 'device'"
          class="sortable"
          (click)="toggleSort('device')"
        >
          Device
          <tui-icon [icon]="getSortIcon('device')" />
        </th>
        <th
          tuiTh
          [class.active]="sortColumn() === 'ports'"
          class="sortable"
          (click)="toggleSort('ports')"
        >
          Port(s)
          <tui-icon [icon]="getSortIcon('ports')" />
        </th>
        <th
          tuiTh
          [class.active]="sortColumn() === 'protocol'"
          class="sortable"
          (click)="toggleSort('protocol')"
        >
          Protocol
          <tui-icon [icon]="getSortIcon('protocol')" />
        </th>
        <th
          tuiTh
          [class.active]="sortColumn() === 'ipVersion'"
          class="sortable"
          (click)="toggleSort('ipVersion')"
        >
          IP Version
          <tui-icon [icon]="getSortIcon('ipVersion')" />
        </th>
        <th
          tuiTh
          [class.active]="sortColumn() === 'source'"
          class="sortable"
          (click)="toggleSort('source')"
        >
          Source
          <tui-icon [icon]="getSortIcon('source')" />
        </th>
        <th tuiTh [style.min-width.rem]="16">Endpoints</th>
        <th tuiTh [style.width.rem]="3"></th>
      </tr>
    </thead>
    <tbody>
      @for (item of sortedItems(); track item.id; let i = $index) {
        <tr>
          <td tuiTd>
            <span
              class="status-icon {{ statusInfo(item).class }}"
              [tuiHint]="statusHint"
              tuiHintDirection="end"
            >
              {{ statusInfo(item).icon }}
            </span>
            <ng-template #statusHint>
              <span class="status-hint">
                <strong>{{ statusInfo(item).label }}</strong>
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
            <div class="endpoints">
              @if (getEndpointIpv4(item); as endpoint) {
                <button class="endpoint" (click)="copyEndpoint(endpoint)">
                  {{ endpoint }}
                  <tui-icon icon="@tui.copy" class="copy-icon" />
                </button>
              }
              @if (getEndpointIpv6(item); as endpoint) {
                <button class="endpoint" (click)="copyEndpoint(endpoint)">
                  {{ endpoint }}
                  <tui-icon icon="@tui.copy" class="copy-icon" />
                </button>
              }
              @if (!getEndpointIpv4(item) && !getEndpointIpv6(item)) {
                <span class="no-endpoint">—</span>
              }
            </div>
          </td>
          <td tuiTd>
            <button
              tuiIconButton
              size="xs"
              iconStart="@tui.ellipsis-vertical"
              appearance="icon"
              [tuiDropdown]="actionsDropdown"
              [(tuiDropdownOpen)]="dropdownOpen[i]"
            >
              Actions
            </button>
            <ng-template #actionsDropdown>
              <tui-data-list size="s">
                <button
                  tuiOption
                  type="button"
                  (click)="toggleEnabled(item); dropdownOpen[i] = false"
                >
                  <tui-icon
                    [icon]="
                      item.enabled ? '@tui.circle-pause' : '@tui.circle-play'
                    "
                  />
                  {{ item.enabled ? 'Disable' : 'Enable' }}
                </button>
                <button
                  tuiOption
                  type="button"
                  (click)="edit.emit(item); dropdownOpen[i] = false"
                >
                  <tui-icon icon="@tui.pencil" />
                  Edit
                </button>
                <button
                  tuiOption
                  type="button"
                  class="delete-option"
                  (click)="delete(item.id); dropdownOpen[i] = false"
                >
                  <tui-icon icon="@tui.trash" />
                  Delete
                </button>
              </tui-data-list>
            </ng-template>
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
    .sortable {
      cursor: pointer;
      user-select: none;

      > tui-icon {
        font-size: 1rem;
        vertical-align: middle;
        margin-left: 0.25rem;
        opacity: 0.4;
        transition: opacity 0.15s;
      }

      &:hover > tui-icon {
        opacity: 0.7;
      }

      &.active > tui-icon {
        opacity: 1;
      }
    }

    th.sortable:first-child {
      > tui-icon {
        margin-left: 0;
      }
    }

    .status-icon {
      cursor: help;
      font-size: 0.875rem;

      &.status-active {
        color: var(--tui-status-positive);
      }

      &.status-partial {
        color: var(--tui-status-warning);
      }

      &.status-paused {
        color: var(--tui-text-secondary);
      }

      &.status-error {
        color: var(--tui-status-negative);
      }

      &.status-disabled {
        color: var(--tui-text-tertiary);
      }
    }

    .status-hint {
      display: flex;
      flex-direction: column;
      gap: 0.25rem;
    }

    .endpoints {
      display: flex;
      flex-direction: column;
      gap: 0.25rem;
      white-space: nowrap;

      .endpoint {
        background: none;
        border: none;
        padding: 0;
        font: inherit;
        color: var(--tui-text-action);
        cursor: pointer;
        text-align: left;
        display: inline-flex;
        align-items: center;
        gap: 0.25rem;

        .copy-icon {
          font-size: 0.875rem;
          opacity: 0;
          transition: opacity 0.15s;
        }

        &:hover .copy-icon {
          opacity: 1;
        }
      }

      .no-endpoint {
        color: var(--tui-text-tertiary);
      }
    }

    [tuiIconButton] {
      margin-block: -0.25rem;
    }

    [tuiOption] {
      display: flex;
      align-items: center;
      gap: 0.5rem;

      tui-icon {
        font-size: 1rem;
      }
    }

    .delete-option {
      color: var(--tui-status-negative);
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
    TuiIcon,
    TuiLink,
    Placeholder,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PublishedPortsTable {
  private readonly clipboard = inject(Clipboard)
  private readonly alerts = inject(TuiNotificationService)

  protected readonly service = injectFormService<PublishedPortDisplay[]>()

  public readonly publishedPortsTable = input<PublishedPortDisplay[]>([])
  public readonly ipv4EndpointHost = input<string | null>(null)
  public readonly edit = output<PublishedPortDisplay>()

  protected dropdownOpen: boolean[] = []
  protected readonly sortColumn = signal<SortColumn>(null)
  protected readonly sortDirection = signal<SortDirection>('asc')

  protected getSortIcon(column: SortColumn): string {
    if (this.sortColumn() !== column) {
      return '@tui.arrow-up-down'
    }
    return this.sortDirection() === 'asc' ? '@tui.arrow-up' : '@tui.arrow-down'
  }

  protected readonly sortedItems = computed(() => {
    const items = this.publishedPortsTable()
    const column = this.sortColumn()
    const direction = this.sortDirection()

    if (!column) return items

    const statusOrder = ['active', 'partial', 'paused', 'error', 'disabled']
    const ipVersionOrder = ['ipv4', 'ipv6', 'both']

    return [...items].sort((a, b) => {
      let compare = 0

      switch (column) {
        case 'status':
          compare =
            statusOrder.indexOf(a.status) - statusOrder.indexOf(b.status)
          break
        case 'name':
          compare = a.label.localeCompare(b.label)
          break
        case 'device':
          compare = (a.deviceName || '').localeCompare(b.deviceName || '')
          break
        case 'protocol':
          compare = a.protocol.localeCompare(b.protocol)
          break
        case 'ipVersion':
          compare =
            ipVersionOrder.indexOf(this.getIpVersion(a)) -
            ipVersionOrder.indexOf(this.getIpVersion(b))
          break
        case 'ports':
          compare = a.ports.localeCompare(b.ports, undefined, { numeric: true })
          break
        case 'source':
          compare = a.source.localeCompare(b.source)
          break
      }

      return direction === 'asc' ? compare : -compare
    })
  })

  protected toggleSort(column: SortColumn) {
    if (this.sortColumn() === column) {
      // Toggle direction or clear
      if (this.sortDirection() === 'asc') {
        this.sortDirection.set('desc')
      } else {
        this.sortColumn.set(null)
      }
    } else {
      this.sortColumn.set(column)
      this.sortDirection.set('asc')
    }
  }

  protected statusInfo(item: PublishedPortDisplay) {
    return STATUS_LABELS[item.status]
  }

  protected protocolLabel(protocol: string) {
    return PROTOCOL_LABELS[protocol as keyof typeof PROTOCOL_LABELS] || protocol
  }

  protected ipVersionLabel(item: PublishedPortDisplay): string {
    const version = this.getIpVersion(item)
    if (version === 'both') return 'IPv4 + IPv6'
    if (version === 'ipv6') return 'IPv6'
    return 'IPv4'
  }

  private getIpVersion(item: PublishedPortDisplay): 'ipv4' | 'ipv6' | 'both' {
    if (item.ipv4 && item.ipv6) return 'both'
    if (item.ipv6) return 'ipv6'
    return 'ipv4'
  }

  protected getEndpointIpv4(item: PublishedPortDisplay): string | null {
    if (!item.ipv4) return null
    const host = this.ipv4EndpointHost()
    if (!host) return null
    const port = item.ipv4PublicPort || item.ports
    return `${host}:${port}`
  }

  protected getEndpointIpv6(item: PublishedPortDisplay): string | null {
    if (!item.ipv6 || !item.deviceIpv6) return null
    // Don't show non-routable addresses:
    // - fe80:: = link-local (not routable)
    // - fd = ULA (local only, requires WAN IPv6 for global address)
    // - :: = incomplete/placeholder
    if (
      item.deviceIpv6.startsWith('fe80:') ||
      item.deviceIpv6.startsWith('fd') ||
      item.deviceIpv6.startsWith('::')
    ) {
      return null
    }
    return `[${item.deviceIpv6}]:${item.ports}`
  }

  protected copyEndpoint(endpoint: string) {
    this.clipboard.copy(endpoint)
    this.alerts.open('Copied', { appearance: 'positive' }).subscribe()
  }

  protected toggleEnabled(item: PublishedPortDisplay) {
    item.enabled = !item.enabled
    item.status = item.enabled ? 'active' : 'disabled'
    this.service.save([...this.publishedPortsTable()])
  }

  protected delete(id: string) {
    const items = this.publishedPortsTable().filter(item => item.id !== id)
    this.service.save(items)
  }
}
