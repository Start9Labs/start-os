import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ErrorService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiDataList, TuiDropdown } from '@taiga-ui/core'
import {
  TUI_CONFIRM,
  TuiNotificationMiddleService,
  TuiSkeleton,
} from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { PlaceholderComponent } from 'src/app/routes/home/components/placeholder'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'
import { DEVICES_ADD } from './add'
import { DEVICES_CONFIG } from './config'
import { MappedDevice } from './utils'

@Component({
  template: `
    <table class="g-table" [tuiSkeleton]="!devices()">
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
                tuiDropdownAuto
                appearance="flat-grayscale"
                iconStart="@tui.ellipsis-vertical"
              >
                Actions
                <tui-data-list
                  *tuiDropdown="let close"
                  size="s"
                  (click)="close()"
                >
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    (click)="onEdit(device)"
                  >
                    Rename
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.settings"
                    (click)="onConfig(device)"
                  >
                    View Config
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.trash"
                    (click)="onDelete(device)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td colspan="4">
              <app-placeholder icon="@tui.laptop">No devices</app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    TuiDropdown,
    TuiDataList,
    PlaceholderComponent,
    TuiSkeleton,
  ],
})
export default class Devices {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)

  protected readonly subnets = toSignal(
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
    { initialValue: null },
  )

  protected readonly devices = computed(() =>
    this.subnets()?.flatMap(subnet =>
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
    const loader = this.loading.open('').subscribe()
    try {
      const data = await this.api.showDeviceConfig({ subnet: subnet.range, ip })

      this.dialogs.open(DEVICES_CONFIG, { data, closable: false }).subscribe()
    } catch (e: any) {
      console.log(e)
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  protected onDelete({ subnet, ip }: MappedDevice): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open('').subscribe()
        try {
          await this.api.deleteDevice({ subnet: subnet.range, ip })
        } catch (e: any) {
          this.errorService.handleError(e)
          console.log(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}
