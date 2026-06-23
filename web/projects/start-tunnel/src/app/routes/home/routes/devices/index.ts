import { Component, computed, inject } from '@angular/core'
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
import {
  defaultWanIp,
  wanLabel,
  wanOptions,
} from 'src/app/routes/home/components/wan'
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
          <th>Autoconfig</th>
          <th>WAN</th>
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
              {{
                device.allowDnsInjection && device.allowAutoPortForward
                  ? 'Yes'
                  : 'No'
              }}
            </td>
            <td>{{ wanLabel(device.wanIp, defaultWan()) }}</td>
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
                    Edit
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
            <td colspan="6">
              <app-placeholder icon="@tui.laptop">No devices</app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      max-inline-size: 50rem;
    }
  `,
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
  private readonly patch = inject<PatchDB<TunnelData>>(PatchDB)

  protected readonly wanLabel = wanLabel

  private readonly wans = toSignal(
    this.patch.watch$('gateways').pipe(map(wanOptions)),
    { initialValue: [] },
  )

  protected readonly defaultWan = toSignal(
    this.patch.watch$('gateways').pipe(map(defaultWanIp)),
    { initialValue: null },
  )

  protected readonly subnets = toSignal(
    this.patch.watch$('wg', 'subnets').pipe(
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
      Object.entries(subnet.clients).map(
        ([ip, { name, allowDnsInjection, allowAutoPortForward, wanIp }]) => ({
          subnet: {
            name: subnet.name,
            range: subnet.range,
          },
          ip,
          name,
          allowDnsInjection,
          allowAutoPortForward,
          wanIp,
        }),
      ),
    ),
  )

  protected onAdd() {
    this.dialogs
      .open(DEVICES_ADD, {
        label: 'Add device',
        data: {
          subnets: this.subnets,
          wanOptions: this.wans(),
          defaultWan: this.defaultWan(),
        },
      })
      .subscribe()
  }

  protected onEdit(device: MappedDevice) {
    this.dialogs
      .open(DEVICES_ADD, {
        label: 'Edit device',
        data: {
          device,
          subnets: this.subnets,
          wanOptions: this.wans(),
          defaultWan: this.defaultWan(),
        },
      })
      .subscribe()
  }

  async onConfig({ subnet, ip }: MappedDevice) {
    const loader = this.loading.open('').subscribe()
    try {
      const data = await this.api.showDeviceConfig({ subnet: subnet.range, ip })

      this.dialogs
        .open(DEVICES_CONFIG, { data, closable: false, size: 'm' })
        .subscribe()
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
