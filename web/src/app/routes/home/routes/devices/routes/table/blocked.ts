import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLink } from '@taiga-ui/core'
import { Placeholder } from 'src/app/components/placeholder'
import { DevicesService } from 'src/app/routes/home/routes/devices/service'
import { DeviceTableItem } from 'src/app/routes/home/routes/devices/utils'

@Component({
  selector: 'table[devicesBlocked]',
  template: `
    <caption [style.background]="'var(--tui-status-negative-pale)'">
      Blocked
    </caption>
    <thead>
      <tr>
        <th tuiTh [sorter]="'name' | tuiSorter">Name</th>
        <th tuiTh>MAC</th>
        <th tuiTh></th>
      </tr>
    </thead>
    <tbody>
      @for (item of devicesBlocked() | tuiTableSort; track item.mac) {
        <tr>
          <td tuiTd>
            <a tuiLink [routerLink]="item.mac">
              <strong>{{ item.name }}</strong>
            </a>
          </td>
          <td tuiTd>{{ item.mac }}</td>
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
              (click)="onUnblock(item.mac)"
            >
              Unblock
            </button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="3">
            <app-placeholder icon="@tui.shield">
              No blocked devices
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
  imports: [RouterLink, TuiTable, TuiButton, TuiLink, Placeholder],
  host: { class: 'g-table' },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DevicesBlocked {
  private readonly service = inject(DevicesService)

  readonly devicesBlocked = input<readonly DeviceTableItem[]>([])

  async onUnblock(mac: string) {
    await this.service.unblock(mac)
  }

  async onForget(mac: string) {
    await this.service.forget(mac)
  }
}
