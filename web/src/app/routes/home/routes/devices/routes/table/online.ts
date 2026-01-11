import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiFormatNumberPipe, TuiIcon, TuiLink } from '@taiga-ui/core'
import { TuiChip } from '@taiga-ui/kit'

import { Device } from './index'

@Component({
  selector: 'table[devicesOnline]',
  template: `
    <caption [style.background]="'var(--tui-status-positive-pale)'">
      Online
    </caption>
    <thead>
      <tr>
        <th tuiTh>Name</th>
        <th tuiTh>Connection</th>
        <th tuiTh>Permissions</th>
        <th tuiTh>MAC</th>
        <th tuiTh>IP</th>
        <th tuiTh>Data</th>
        <th tuiTh [style.text-align]="'start'">Speed</th>
      </tr>
    </thead>
    <tbody>
      @for (item of devicesOnline(); track $index) {
        <tr>
          <td tuiTd>
            <a tuiLink routerLink="id">
              <strong>{{ item.name }}</strong>
            </a>
          </td>
          <td tuiTd>
            <div tuiChip size="xs" iconStart="@tui.wifi">
              {{ item.connection }}
            </div>
          </td>
          <td tuiTd>
            {{ item.permission }}
          </td>
          <td tuiTd>{{ item.mac }}</td>
          <td tuiTd>
            @for (ip of item.ip; track $index) {
              <div tuiChip size="xs" iconStart="@tui.lock">{{ ip }}</div>
            }
          </td>
          <td tuiTd>
            {{ item.data ?? 0 | tuiFormatNumber | async }}
            <small class="g-secondary">GB</small>
          </td>
          <td tuiTd>
            @for (speed of item.speed; track $index) {
              <div
                tuiChip
                size="xs"
                [iconStart]="$index ? '@tui.arrow-down' : '@tui.arrow-up'"
              >
                <span>
                  {{ speed | tuiFormatNumber | async }}
                  <small class="g-secondary">MB/S</small>
                </span>
              </div>
            }
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="7">
            <tui-icon icon="@tui.monitor-smartphone" />
            No online devices
          </td>
        </tr>
      }
    </tbody>
  `,
  imports: [
    AsyncPipe,
    RouterLink,
    TuiTable,
    TuiIcon,
    TuiFormatNumberPipe,
    TuiChip,
    TuiLink,
  ],
  host: { class: 'g-table' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesOnline {
  readonly devicesOnline = input<readonly Device[]>([])
}
