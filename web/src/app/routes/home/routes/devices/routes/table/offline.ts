import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
  signal,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiFormatNumberPipe,
  TuiIcon,
  TuiLink,
} from '@taiga-ui/core'
import { TuiChip } from '@taiga-ui/kit'
import { DevicesService } from 'src/app/routes/home/routes/devices/service'
import { DeviceTableItem } from 'src/app/routes/home/routes/devices/utils'

type SortDirection = 'asc' | 'desc' | null

@Component({
  selector: 'table[devicesOffline]',
  template: `
    <caption [style.background]="'var(--tui-background-neutral-2)'">
      Offline
    </caption>
    <thead>
      <tr>
        <th tuiTh class="sortable" (click)="toggleSort()">
          Name
          <tui-icon [icon]="sortIcon()" />
        </th>
        <th tuiTh>MAC</th>
        <th tuiTh>IP</th>
        <th tuiTh>Data</th>
        <th tuiTh></th>
      </tr>
    </thead>
    <tbody>
      @for (item of sortedDevices(); track item.mac) {
        <tr>
          <td tuiTd>
            <a tuiLink [routerLink]="item.mac">
              <strong>{{ item.name }}</strong>
            </a>
          </td>
          <td tuiTd>{{ item.mac }}</td>
          <td tuiTd>
            @if (item.ipv4) {
              <div
                tuiChip
                size="xs"
                [iconStart]="item.ipv4Static ? '@tui.lock' : ''"
              >
                {{ item.ipv4 }}
              </div>
            }
            @if (item.ipv6) {
              <div
                tuiChip
                size="xs"
                [iconStart]="item.ipv6Static ? '@tui.lock' : ''"
              >
                {{ item.ipv6 }}
              </div>
            }
          </td>
          <td tuiTd>
            {{ item.dataUsage ?? 0 | tuiFormatNumber }}
            <small class="g-secondary">GB</small>
          </td>
          <td tuiTd class="actions">
            <button
              appearance="secondary"
              size="xs"
              tuiButton
              (click)="onForget(item.mac)"
            >
              Forget
            </button>
            <button
              appearance="secondary-destructive"
              size="xs"
              tuiButton
              (click)="onBlock(item.mac)"
            >
              Block
            </button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="5">
            <tui-icon icon="@tui.monitor-smartphone" />
            No offline devices
          </td>
        </tr>
      }
    </tbody>
  `,
  styles: `
    .sortable {
      cursor: pointer;
      user-select: none;

      tui-icon {
        font-size: 1rem;
        vertical-align: middle;
        margin-left: 0.25rem;
      }
    }

    .actions {
      text-align: right;

      button + button {
        margin-left: 0.5rem;
      }
    }
  `,
  imports: [
    RouterLink,
    TuiTable,
    TuiIcon,
    TuiButton,
    TuiFormatNumberPipe,
    TuiChip,
    TuiLink,
  ],
  host: { class: 'g-table' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesOffline {
  private readonly service = inject(DevicesService)

  readonly devicesOffline = input<readonly DeviceTableItem[]>([])

  protected readonly sortDirection = signal<SortDirection>(null)

  protected readonly sortIcon = computed(() => {
    switch (this.sortDirection()) {
      case 'asc':
        return '@tui.arrow-up'
      case 'desc':
        return '@tui.arrow-down'
      default:
        return '@tui.arrow-up-down'
    }
  })

  protected readonly sortedDevices = computed(() => {
    const devices = this.devicesOffline()
    const direction = this.sortDirection()
    if (!direction) return devices

    return [...devices].sort((a, b) => {
      const compare = a.name.localeCompare(b.name)
      return direction === 'asc' ? compare : -compare
    })
  })

  protected toggleSort() {
    const current = this.sortDirection()
    if (current === null) {
      this.sortDirection.set('asc')
    } else if (current === 'asc') {
      this.sortDirection.set('desc')
    } else {
      this.sortDirection.set(null)
    }
  }

  async onBlock(mac: string) {
    await this.service.block(mac)
  }

  async onForget(mac: string) {
    await this.service.forget(mac)
  }
}
