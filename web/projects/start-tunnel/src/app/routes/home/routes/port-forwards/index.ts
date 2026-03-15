import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
  Signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
import { ErrorService } from '@start9labs/shared'
import { utils } from '@start9labs/start-sdk'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiLoader,
  TuiTextfield,
} from '@taiga-ui/core'
import {
  TUI_CONFIRM,
  TuiNotificationMiddleService,
  TuiSwitch,
} from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { PORT_FORWARDS_ADD } from 'src/app/routes/home/routes/port-forwards/add'
import { PORT_FORWARDS_EDIT_LABEL } from 'src/app/routes/home/routes/port-forwards/edit-label'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

import { MappedDevice, MappedForward } from './utils'

@Component({
  template: `
    <table class="g-table">
      <thead>
        <tr>
          <th></th>
          <th>Label</th>
          <th>External IP</th>
          <th>External Port</th>
          <th>Device</th>
          <th>Internal Port</th>
          <th [style.padding-inline-end.rem]="0.625">
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (forward of forwards(); track $index) {
          <tr>
            <td>
              <tui-loader
                [loading]="toggling() === $index"
                size="xs"
                [overlay]="true"
              >
                <input
                  tuiSwitch
                  type="checkbox"
                  size="s"
                  [showIcons]="false"
                  [ngModel]="forward.enabled"
                  (ngModelChange)="onToggle(forward, $index)"
                />
              </tui-loader>
            </td>
            <td>{{ forward.label || '—' }}</td>
            <td>{{ forward.externalip }}</td>
            <td>{{ forward.externalport }}</td>
            <td>{{ forward.device.name }}</td>
            <td>{{ forward.internalport }}</td>
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
                <tui-data-list *tuiDropdown size="s">
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    new
                    (click)="onEditLabel(forward)"
                  >
                    {{ forward.label ? 'Rename' : 'Add label' }}
                  </button>
                  <button
                    tuiOption
                    iconStart="@tui.trash"
                    new
                    (click)="onDelete(forward)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        } @empty {
          <div class="placeholder">No port forwards</div>
        }
      </tbody>
    </table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    FormsModule,
    TuiButton,
    TuiDropdown,
    TuiDataList,
    TuiLoader,
    TuiSwitch,
    TuiTextfield,
  ],
})
export default class PortForwards {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly patch = inject<PatchDB<TunnelData>>(PatchDB)
  private readonly errorService = inject(ErrorService)

  private readonly portForwards = toSignal(this.patch.watch$('portForwards'))
  private readonly ips = toSignal(
    this.patch.watch$('gateways').pipe(
      map(g =>
        Object.values(g)
          .flatMap(
            val => val.ipInfo?.subnets.map(s => utils.IpNet.parse(s)) || [],
          )
          .filter(s => s.isIpv4() && s.isPublic())
          .map(s => s.address),
      ),
    ),
    { initialValue: [] },
  )

  private readonly devices: Signal<MappedDevice[]> = toSignal(
    this.patch
      .watch$('wg', 'subnets')
      .pipe(
        map(subnets =>
          Object.values(subnets).flatMap(({ clients }) =>
            Object.entries(clients).map(([ip, { name }]) => ({ ip, name })),
          ),
        ),
      ),
    { initialValue: [] },
  )

  protected readonly forwards = computed(() =>
    Object.entries(this.portForwards() || {}).map(([source, entry]) => {
      const sourceSplit = source.split(':')
      const targetSplit = entry.target.split(':')

      return {
        externalip: sourceSplit[0]!,
        externalport: sourceSplit[1]!,
        device: this.devices().find(d => d.ip === targetSplit[0])!,
        internalport: targetSplit[1]!,
        label: entry.label,
        enabled: entry.enabled,
      }
    }),
  )

  protected readonly toggling = signal<number | null>(null)

  protected async onToggle(forward: MappedForward, index: number) {
    this.toggling.set(index)
    const source = `${forward.externalip}:${forward.externalport}`

    try {
      await this.api.setForwardEnabled({ source, enabled: !forward.enabled })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.toggling.set(null)
    }
  }

  protected onAdd(): void {
    this.dialogs
      .open(PORT_FORWARDS_ADD, {
        label: 'Add port forward',
        data: { ips: this.ips, devices: this.devices },
      })
      .subscribe()
  }

  protected onEditLabel(forward: MappedForward): void {
    this.dialogs
      .open(PORT_FORWARDS_EDIT_LABEL, {
        label: 'Edit label',
        data: {
          source: `${forward.externalip}:${forward.externalport}`,
          label: forward.label,
        },
      })
      .subscribe()
  }

  protected onDelete({ externalip, externalport }: MappedForward): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open('').subscribe()
        const source = `${externalip}:${externalport}`

        try {
          await this.api.deleteForward({ source })
        } catch (e: any) {
          console.log(e)
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}
