import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiIcon, TuiLink } from '@taiga-ui/core'

import { Device } from './index'

@Component({
  selector: 'table[devicesBlocked]',
  template: `
    <caption [style.background]="'var(--tui-status-negative-pale)'">
      Blocked
    </caption>
    <thead>
      <tr>
        <th tuiTh>Name</th>
        <th tuiTh>MAC</th>
        <th tuiTh></th>
      </tr>
    </thead>
    <tbody>
      @for (item of devicesBlocked(); track $index) {
        <tr>
          <td tuiTd>
            <a tuiLink routerLink="id">
              <strong>{{ item.name }}</strong>
            </a>
          </td>
          <td tuiTd>{{ item.mac }}</td>
          <td tuiTd>
            <button appearance="outline" size="xs" tuiButton>Unblock</button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="3">
            <tui-icon icon="@tui.monitor-smartphone" />
            No blocked devices
          </td>
        </tr>
      }
    </tbody>
  `,
  imports: [RouterLink, TuiTable, TuiIcon, TuiButton, TuiLink],
  host: { class: 'g-table' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesBlocked {
  readonly devicesBlocked = input<readonly Device[]>([])
}
