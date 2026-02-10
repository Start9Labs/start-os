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
  selector: 'table[devicesOffline]',
  template: `
    <caption [style.background]="'var(--tui-background-neutral-2)'">
      Offline
    </caption>
    <thead>
      <tr>
        <th tuiTh [sorter]="'name' | tuiSorter">Name</th>
        <th tuiTh>MAC</th>
        <th tuiTh>IP</th>
        <th tuiTh>Data</th>
        <th tuiTh></th>
      </tr>
    </thead>
    <tbody>
      @for (item of devicesOffline() | tuiTableSort; track item.mac) {
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
            <app-placeholder icon="@tui.screen-share-off">
              No offline devices
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
export class DevicesOffline {
  private readonly service = inject(DevicesService)

  readonly devicesOffline = input<readonly DeviceTableItem[]>([])

  async onBlock(mac: string) {
    await this.service.block(mac)
  }

  async onForget(mac: string) {
    await this.service.forget(mac)
  }
}
