import {
  ChangeDetectionStrategy,
  Component,
  computed,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiTextfield, TuiTitle } from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help'

import { DevicesAside } from './aside'
import { DevicesBlocked } from './blocked'
import { DevicesOffline } from './offline'
import { DevicesOnline } from './online'

export interface Device {
  name: string
  mac: string
  connection?: string
  permission?: string
  data?: number
  ip?: [string, string]
  speed?: [number, number]
}

@Component({
  template: `
    <devices-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Devices</h2></hgroup>
      <aside tuiAccessories>
        <tui-textfield tuiTextfieldSize="s" iconStart="@tui.search">
          <input
            tuiTextfield
            placeholder="Search devices"
            [(ngModel)]="value"
          />
        </tui-textfield>
      </aside>
    </header>
    <table tuiTable [devicesOnline]="online()"></table>
    <table tuiTable [devicesOffline]="offline()"></table>
    <table tuiTable [devicesBlocked]="blocked()"></table>
  `,
  styles: `
    aside {
      max-width: 21rem;
      flex: 8;
    }

    tui-textfield {
      width: 100%;
    }

    table {
      margin-bottom: 1rem;

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
  imports: [
    FormsModule,
    TuiHeader,
    TuiTitle,
    TuiTextfield,
    TuiTable,
    Help,
    DevicesAside,
    DevicesOnline,
    DevicesOffline,
    DevicesBlocked,
  ],
})
export default class DevicesTable {
  private readonly data = signal({
    online: [
      {
        name: 'humble-weeds',
        connection: 'Eth1',
        permission: 'Admin',
        mac: '00:1A:2B:3C:4D:5E',
        ip: ['192.168.12.21', 'fe80::1ff:fe23:4567:890a'],
        data: 12.5,
        speed: [35, 84.73],
      },
      {
        name: 'Pixel',
        connection: 'Child',
        permission: 'Guest',
        mac: '00:1A:2B:3C:4D:5E',
        ip: ['192.168.12.21', 'fe80::1ff:fe23:4567:890a'],
        data: 0.7,
        speed: [50, 4],
      },
    ] satisfies readonly Device[],
    offline: [
      {
        name: 'iPhone13',
        mac: 'DE:AD:BE:EF:CA:FE',
        permission: 'Guest',
        data: 5.1,
        ip: ['192.168.1.1', 'fe80::1ff:fe23:4567:890a'],
      },
      {
        name: "Mariusz's phone",
        mac: 'DE:AD:BE:EF:CA:FE',
        data: 1.2,
        ip: ['192.168.1.1', 'fe80::1ff:fe23:4567:890a'],
      },
    ] satisfies readonly Device[],
    blocked: [
      {
        name: "Bob's Computer",
        mac: '01:23:45:67:89:AB',
      },
    ],
  })

  protected readonly value = signal('')

  protected readonly online = computed(() =>
    this.filter(this.data().online, this.value()),
  )

  protected readonly offline = computed(() =>
    this.filter(this.data().offline, this.value()),
  )

  protected readonly blocked = computed(() =>
    this.filter(this.data().blocked, this.value()),
  )

  private filter(devices: readonly Device[], value: string): readonly Device[] {
    return devices.filter(device =>
      Object.values(device).some(field =>
        String(field).toLowerCase().includes(value.toLowerCase()),
      ),
    )
  }
}
