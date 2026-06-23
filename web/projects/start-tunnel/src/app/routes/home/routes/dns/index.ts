import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Signal,
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
import { MappedDevice } from 'src/app/routes/home/routes/port-forwards/utils'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

import { DNS_ADD } from './add'

@Component({
  template: `
    <table class="g-table" [tuiSkeleton]="!records()">
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Value</th>
          <th>TTL</th>
          <th>Source</th>
          <th [style.padding-inline-end.rem]="0.625">
            <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
              Add
            </button>
          </th>
        </tr>
      </thead>
      <tbody>
        @for (record of records() || []; track $index) {
          <tr>
            <td>{{ record.name }}</td>
            <td>{{ record.type }}</td>
            <td>{{ record.value }}</td>
            <td>{{ record.ttl }}</td>
            <td>{{ record.source || 'Manual' }}</td>
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
                    iconStart="@tui.trash"
                    (click)="onDelete(record)"
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
              <app-placeholder icon="@tui.list">
                No DNS records. Devices you trust can add their own via RFC
                2136, or add one manually.
              </app-placeholder>
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
export default class Dns {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly api = inject(ApiService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly patch = inject<PatchDB<TunnelData>>(PatchDB)
  private readonly errorService = inject(ErrorService)

  protected readonly records = toSignal(this.patch.watch$('dnsRecords'))

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

  protected onAdd(): void {
    this.dialogs
      .open(DNS_ADD, {
        label: 'Add DNS record',
        data: { devices: this.devices },
      })
      .subscribe()
  }

  protected onDelete(record: { name: string; type: string }): void {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loading.open('').subscribe()
        try {
          await this.api.removeDnsRecord({
            name: record.name,
            type: record.type,
          })
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}
