import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiFormatNumberPipe,
  TuiIcon,
  TuiLink,
} from '@taiga-ui/core'
import { TuiChip } from '@taiga-ui/kit'

import { Device } from './index'

@Component({
  selector: 'table[devicesOffline]',
  template: `
    <caption [style.background]="'var(--tui-background-base-alt)'">
      Offline
    </caption>
    <thead>
      <tr>
        <th tuiTh>Name</th>
        <th tuiTh>Permissions</th>
        <th tuiTh>MAC</th>
        <th tuiTh>IP</th>
        <th tuiTh>Data</th>
        <th tuiTh></th>
      </tr>
    </thead>
    <tbody>
      @for (item of devicesOffline(); track $index) {
        <tr>
          <td tuiTd>
            <a tuiLink routerLink="id">
              <strong>{{ item.name }}</strong>
            </a>
          </td>
          <td tuiTd>
            {{ item.permission || '-' }}
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
            <button appearance="outline-destructive" size="xs" tuiButton>
              Forget
            </button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="6">
            <tui-icon icon="@tui.monitor-smartphone" />
            No offline devices found
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
    TuiButton,
    TuiFormatNumberPipe,
    TuiChip,
    TuiLink,
  ],
  host: { class: 'g-table' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesOffline {
  readonly devicesOffline = input<readonly Device[]>([])
}
