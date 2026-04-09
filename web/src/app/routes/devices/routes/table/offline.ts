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
import { DevicesService } from 'src/app/routes/devices/service'
import { DeviceTableItem } from 'src/app/routes/devices/utils'

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
      @for (
        item of devicesOffline() | tuiTableSort;
        track item.mac ?? item.ipv4
      ) {
        <tr>
          <td tuiTd>
            @if (item.mac) {
              <a tuiLink routerLink="device" [queryParams]="{ mac: item.mac }">
                <strong>{{ item.name }}</strong>
              </a>
            } @else {
              <strong>{{ item.name }}</strong>
            }
          </td>
          <td tuiTd>{{ item.mac || '-' }}</td>
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
            @if (item.mac) {
              <button
                appearance="secondary"
                size="xs"
                tuiButton
                (click)="onForget(item.mac)"
              >
                Forget
              </button>
            }
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

  async onForget(mac: string) {
    await this.service.forget(mac)
  }
}
