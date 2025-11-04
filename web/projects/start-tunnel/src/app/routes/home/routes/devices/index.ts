import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  Signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { LoadingService } from '@start9labs/shared'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiTextfield,
} from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

import { DEVICES_ADD } from './add'
import { DEVICES_CONFIG } from './config'
import { MappedDevice, MappedSubnet } from './utils'

@Component({
  template: `
    <table class="g-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Subnet</th>
          <th>LAN IP</th>
          <th [style.padding-inline-end.rem]="0.625">
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (device of devices(); track $index) {
          <tr>
            <td>{{ device.name }}</td>
            <td>{{ device.subnet.name }}</td>
            <td>{{ device.ip }}</td>
            <td>
              <button
                tuiIconButton
                size="xs"
                tuiDropdown
                tuiDropdownOpen
                appearance="flat-grayscale"
                iconStart="@tui.ellipsis-vertical"
              >
                Actions
                <tui-data-list *tuiTextfieldDropdown size="s">
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    new
                    (click)="onEdit(device)"
                  >
                    Rename
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.settings"
                    new
                    (click)="onConfig(device)"
                  >
                    View Config
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.trash"
                    new
                    (click)="onDelete(device)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiDropdown, TuiDataList, TuiTextfield],
})
export default class Devices {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(LoadingService)

  protected readonly subnets: Signal<readonly MappedSubnet[]> = toSignal(
    inject<PatchDB<TunnelData>>(PatchDB)
      .watch$('wg', 'subnets')
      .pipe(
        map(subnets =>
          Object.entries(subnets).map(([range, { name, clients }]) => ({
            range,
            name,
            clients,
          })),
        ),
      ),
    { initialValue: [] },
  )

  protected readonly devices = computed(() =>
    this.subnets().flatMap(subnet =>
      Object.entries(subnet.clients).map(([ip, { name }]) => ({
        subnet: {
          name: subnet.name,
          range: subnet.range,
        },
        ip,
        name,
      })),
    ),
  )

  protected onAdd() {
    this.dialogs
      .open(DEVICES_ADD, {
        label: 'Add device',
        data: { subnets: this.subnets },
      })
      .subscribe()
  }

  protected onEdit(device: MappedDevice) {
    this.dialogs
      .open(DEVICES_ADD, {
        label: 'Rename device',
        data: { device, subnets: this.subnets },
      })
      .subscribe()
  }

  async onConfig({ subnet, ip }: MappedDevice) {
    const loader = this.loading.open().subscribe()
    try {
      const data = await this.api.showDeviceConfig({ subnet: subnet.range, ip })

      this.dialogs.open(DEVICES_CONFIG, { data }).subscribe()
    } catch (e) {
      console.log(e)
    } finally {
      loader.unsubscribe()
    }
  }

  protected onDelete({ subnet, ip }: MappedDevice): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open().subscribe()
        try {
          await this.api.deleteDevice({ subnet: subnet.range, ip })
        } catch (e) {
          console.log(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}
