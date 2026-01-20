import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiInput, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help'
import { DevicesService } from 'src/app/routes/home/routes/devices/service'
import { DeviceTableItem } from 'src/app/routes/home/routes/devices/utils'

import { DevicesAside } from './aside'
import { DevicesBlocked } from './blocked'
import { DevicesOffline } from './offline'
import { DevicesOnline } from './online'

@Component({
  template: `
    <devices-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Devices</h2></hgroup>
      <aside tuiAccessories>
        <tui-textfield tuiTextfieldSize="s" iconStart="@tui.search">
          <input tuiInput placeholder="Search devices" [(ngModel)]="search" />
        </tui-textfield>
      </aside>
    </header>
    <table
      tuiTable
      [devicesOnline]="online()"
      [tuiSkeleton]="loading()"
    ></table>
    <table
      tuiTable
      [devicesOffline]="offline()"
      [tuiSkeleton]="loading()"
    ></table>
    <table
      tuiTable
      [devicesBlocked]="blocked()"
      [tuiSkeleton]="loading()"
    ></table>
  `,
  styles: `
    :host {
      padding-top: 0;
    }

    aside {
      max-width: 21rem;
      flex: 8;
    }

    tui-textfield {
      width: 100%;
    }

    table {
      ::ng-deep {
        td[colspan] {
          text-align: center;
          color: var(--tui-text-tertiary);
          padding: 0.75rem;
        }

        [tuiChip] {
          display: flex;
          background: none;
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    FormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiTable,
    TuiSkeleton,
    TuiInput,
    Help,
    DevicesAside,
    DevicesOnline,
    DevicesOffline,
    DevicesBlocked,
  ],
})
export default class DevicesTable {
  protected readonly service = inject(DevicesService)
  protected readonly search = signal('')

  protected readonly loading = computed(() => !this.service.data())

  protected readonly online = computed(() =>
    this.filter(
      this.service.data()?.filter(d => d.status === 'online') ?? [],
      this.search(),
    ),
  )

  protected readonly offline = computed(() =>
    this.filter(
      this.service.data()?.filter(d => d.status === 'offline') ?? [],
      this.search(),
    ),
  )

  protected readonly blocked = computed(() =>
    this.filter(
      this.service.data()?.filter(d => d.status === 'blocked') ?? [],
      this.search(),
    ),
  )

  private filter(
    devices: readonly DeviceTableItem[],
    value: string,
  ): readonly DeviceTableItem[] {
    if (!value) return devices
    const lower = value.toLowerCase()
    return devices.filter(device =>
      [device.name, device.mac, device.ipv4, device.ipv6, device.connection]
        .filter(Boolean)
        .some(field => field!.toLowerCase().includes(lower)),
    )
  }
}
