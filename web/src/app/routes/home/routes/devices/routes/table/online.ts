import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiFormatNumberPipe, TuiLink } from '@taiga-ui/core'
import { TuiChip } from '@taiga-ui/kit'
import { Placeholder } from 'src/app/components/placeholder'
import { DevicesService } from 'src/app/routes/home/routes/devices/service'
import { DeviceTableItem } from 'src/app/routes/home/routes/devices/utils'

@Component({
  selector: 'table[devicesOnline]',
  template: `
    <caption [style.background]="'var(--tui-status-positive-pale)'">
      Online
    </caption>
    <thead>
      <tr>
        <th tuiTh [sorter]="'name' | tuiSorter">Name</th>
        <th tuiTh>Connection</th>
        <th tuiTh>Security Profile</th>
        <th tuiTh>MAC</th>
        <th tuiTh>IP</th>
        <th tuiTh>Data</th>
        <th tuiTh [style.text-align]="'start'">Speed</th>
        <th tuiTh></th>
      </tr>
    </thead>
    <tbody>
      @for (item of devicesOnline() | tuiTableSort; track item.mac) {
        <tr>
          <td tuiTd>
            <a tuiLink [routerLink]="item.mac">
              <strong>{{ item.name }}</strong>
            </a>
          </td>
          <td tuiTd>
            <div
              tuiChip
              size="xs"
              [iconStart]="getConnectionIcon(item.connection)"
            >
              {{ item.connection }}
            </div>
          </td>
          <td tuiTd>
            <a tuiLink routerLink="/profiles">
              {{ item.securityProfile || 'Default' }}
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
          <td tuiTd>
            @if (item.speed) {
              <div tuiChip size="xs" iconStart="@tui.arrow-up">
                <span>
                  {{ item.speed.up | tuiFormatNumber }}
                  <small class="g-secondary">MB/s</small>
                </span>
              </div>
              <div tuiChip size="xs" iconStart="@tui.arrow-down">
                <span>
                  {{ item.speed.down | tuiFormatNumber }}
                  <small class="g-secondary">MB/s</small>
                </span>
              </div>
            }
          </td>
          <td tuiTd class="actions">
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
          <td colspan="8">
            <app-placeholder icon="@tui.monitor-smartphone">
              No online devices
            </app-placeholder>
          </td>
        </tr>
      }
    </tbody>
  `,
  styles: `
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
    TuiButton,
    TuiFormatNumberPipe,
    TuiChip,
    TuiLink,
    Placeholder,
  ],
  host: { class: 'g-table' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesOnline {
  private readonly service = inject(DevicesService)

  readonly devicesOnline = input<readonly DeviceTableItem[]>([])

  protected getConnectionIcon(connection?: string): string {
    if (!connection) return '@tui.monitor'
    const lower = connection.toLowerCase()
    if (lower.includes('ethernet') || lower.includes('eth')) {
      return '@tui.cable'
    }
    if (lower.includes('wi-fi') || lower.includes('wifi')) {
      return '@tui.wifi'
    }
    return '@tui.monitor'
  }

  async onBlock(mac: string) {
    await this.service.block(mac)
  }
}
