import { Component, computed, inject, signal } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiDataList, TuiDropdown, TuiLoader } from '@taiga-ui/core'
import {
  TUI_CONFIRM,
  TuiNotificationMiddleService,
  TuiSkeleton,
  TuiSwitch,
} from '@taiga-ui/kit'
import { TuiCardLarge } from '@taiga-ui/layout'
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
    <div tuiCardLarge="compact" appearance="floating">
      <header>
        <h3>Servers</h3>
        <button
          tuiButton
          size="xs"
          iconStart="@tui.plus"
          (click)="onAdd('server')"
        >
          Add
        </button>
      </header>
      <table class="g-table" [tuiSkeleton]="!servers()">
        <thead>
          <tr>
            <th>Name</th>
            <th>Subnet</th>
            <th>LAN IP</th>
            <th>DNS Injection</th>
            <th>Auto Port Forward</th>
            <th>WAN</th>
            <th [style.padding-inline-end.rem]="0.625"></th>
          </tr>
        </thead>
        <tbody>
          @for (device of servers(); track $index) {
            <tr>
              <td>{{ device.name }}</td>
              <td>{{ device.subnet.name }}</td>
              <td>{{ device.ip }}</td>
              <td>
                <tui-loader
                  size="xs"
                  [loading]="togglingDns() === device.ip"
                  [overlay]="true"
                >
                  <input
                    tuiSwitch
                    type="checkbox"
                    size="s"
                    [style.display]="'flex'"
                    [showIcons]="false"
                    [ngModel]="device.allowDnsInjection"
                    (ngModelChange)="onDnsInjection(device)"
                  />
                </tui-loader>
              </td>
              <td>
                <tui-loader
                  size="xs"
                  [loading]="togglingPf() === device.ip"
                  [overlay]="true"
                >
                  <input
                    tuiSwitch
                    type="checkbox"
                    size="s"
                    [style.display]="'flex'"
                    [showIcons]="false"
                    [ngModel]="device.allowAutoPortForward"
                    (ngModelChange)="onAutoPortForward(device)"
                  />
                </tui-loader>
              </td>
              <td>{{ wanLabel(device.wanIp, 'Use Subnet Default') }}</td>
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
                      iconStart="@tui.arrow-down"
                      (click)="onSetKind(device, 'client')"
                    >
                      Demote to Client
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
              <td colspan="7">
                <app-placeholder icon="@tui.laptop">No servers</app-placeholder>
              </td>
            </tr>
          }
        </tbody>
      </table>
    </div>

    <div tuiCardLarge="compact" appearance="floating">
      <header>
        <h3>Clients</h3>
        <button
          tuiButton
          size="xs"
          iconStart="@tui.plus"
          (click)="onAdd('client')"
        >
          Add
        </button>
      </header>
      <table class="g-table" [tuiSkeleton]="!clients()">
        <thead>
          <tr>
            <th>Name</th>
            <th>Subnet</th>
            <th>LAN IP</th>
            <th>WAN</th>
            <th [style.padding-inline-end.rem]="0.625"></th>
          </tr>
        </thead>
        <tbody>
          @for (device of clients(); track $index) {
            <tr>
              <td>{{ device.name }}</td>
              <td>{{ device.subnet.name }}</td>
              <td>{{ device.ip }}</td>
              <td>{{ wanLabel(device.wanIp, 'Use Subnet Default') }}</td>
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
                      iconStart="@tui.arrow-up"
                      (click)="onSetKind(device, 'server')"
                    >
                      Promote to Server
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
              <td colspan="5">
                <app-placeholder icon="@tui.laptop">No clients</app-placeholder>
              </td>
            </tr>
          }
        </tbody>
      </table>
    </div>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    header {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    header h3 {
      margin: 0;
    }

    header button {
      margin-inline-start: auto;
    }
  `,
  imports: [
    FormsModule,
    TuiButton,
    TuiCardLarge,
    TuiDropdown,
    TuiDataList,
    TuiLoader,
    TuiSwitch,
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

  protected readonly togglingDns = signal<string | null>(null)
  protected readonly togglingPf = signal<string | null>(null)

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
        ([
          ip,
          { name, kind, allowDnsInjection, allowAutoPortForward, wanIp },
        ]) => ({
          subnet: {
            name: subnet.name,
            range: subnet.range,
          },
          ip,
          name,
          kind,
          allowDnsInjection,
          allowAutoPortForward,
          wanIp,
        }),
      ),
    ),
  )

  protected readonly servers = computed(() =>
    this.devices()?.filter(d => d.kind === 'server'),
  )

  protected readonly clients = computed(() =>
    this.devices()?.filter(d => d.kind === 'client'),
  )

  protected onAdd(kind: T.Tunnel.WgClientKind) {
    this.dialogs
      .open(DEVICES_ADD, {
        label: kind === 'server' ? 'Add server' : 'Add client',
        data: {
          kind,
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

  protected async onDnsInjection({
    subnet,
    ip,
    allowDnsInjection,
  }: MappedDevice) {
    this.togglingDns.set(ip)
    try {
      await this.api.setDnsInjection({
        subnet: subnet.range,
        ip,
        enabled: !allowDnsInjection,
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.togglingDns.set(null)
    }
  }

  protected async onAutoPortForward({
    subnet,
    ip,
    allowAutoPortForward,
  }: MappedDevice) {
    this.togglingPf.set(ip)
    try {
      await this.api.setAutoPortForward({
        subnet: subnet.range,
        ip,
        enabled: !allowAutoPortForward,
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.togglingPf.set(null)
    }
  }

  protected onSetKind(
    { subnet, ip }: MappedDevice,
    kind: T.Tunnel.WgClientKind,
  ): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open('').subscribe()
        try {
          await this.api.setDeviceKind({ subnet: subnet.range, ip, kind })
        } catch (e: any) {
          this.errorService.handleError(e)
          console.log(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}
