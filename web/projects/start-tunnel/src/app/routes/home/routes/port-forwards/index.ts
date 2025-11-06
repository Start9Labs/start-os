import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  Signal,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ReactiveFormsModule } from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { utils } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { PORT_FORWARDS_ADD } from 'src/app/routes/home/routes/port-forwards/add'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

import { MappedDevice, MappedForward } from './utils'

@Component({
  template: `
    <table class="g-table">
      <thead>
        <tr>
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
            <td>{{ forward.externalip }}</td>
            <td>{{ forward.externalport }}</td>
            <td>{{ forward.device.name }}</td>
            <td>{{ forward.internalport }}</td>
            <td>
              <button
                tuiIconButton
                size="xs"
                appearance="flat-grayscale"
                iconStart="@tui.trash"
                (click)="onDelete(forward)"
              >
                Actions
              </button>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ReactiveFormsModule, TuiButton],
})
export default class PortForwards {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(LoadingService)
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
    Object.entries(this.portForwards() || {}).map(([source, target]) => {
      const sourceSplit = source.split(':')
      const targetSplit = target.split(':')

      return {
        externalip: sourceSplit[0]!,
        externalport: sourceSplit[1]!,
        device: this.devices().find(d => d.ip === targetSplit[0])!,
        internalport: targetSplit[1]!,
      }
    }),
  )

  protected onAdd(): void {
    this.dialogs
      .open(PORT_FORWARDS_ADD, {
        label: 'Add port forward',
        data: { ips: this.ips, devices: this.devices },
      })
      .subscribe()
  }

  protected onDelete({ externalip, externalport }: MappedForward): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open().subscribe()
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
