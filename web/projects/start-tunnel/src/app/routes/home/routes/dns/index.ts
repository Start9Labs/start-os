import {
  ChangeDetectionStrategy,
  Component,
  computed,
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
import { TuiCardLarge } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { PlaceholderComponent } from 'src/app/routes/home/components/placeholder'
import { MappedDevice } from 'src/app/routes/home/routes/port-forwards/utils'
import { ApiService } from 'src/app/services/api/api.service'
import { TunnelData } from 'src/app/services/patch-db/data-model'

import { DNS_ADD } from './add'

@Component({
  template: `
    <div tuiCardLarge="compact" appearance="floating">
      <header>
        <h3>Manual</h3>
        <button tuiButton size="xs" iconStart="@tui.plus" (click)="onAdd()">
          Add
        </button>
      </header>
      <table class="g-table" [tuiSkeleton]="!records()">
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Value</th>
            <th>TTL</th>
            <th [style.padding-inline-end.rem]="0.625"></th>
          </tr>
        </thead>
        <tbody>
          @for (record of manual(); track $index) {
            <tr>
              <td>{{ record.name }}</td>
              <td>{{ record.type }}</td>
              <td>{{ record.value }}</td>
              <td>{{ record.ttl }}</td>
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
              <td colspan="5">
                <app-placeholder icon="@tui.list">
                  No manual DNS records. Add one to get started.
                </app-placeholder>
              </td>
            </tr>
          }
        </tbody>
      </table>
    </div>

    <div tuiCardLarge="compact" appearance="floating">
      <header>
        <h3>Automatic</h3>
      </header>
      <table class="g-table" [tuiSkeleton]="!records()">
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Value</th>
            <th>TTL</th>
            <th>Source</th>
            <th [style.padding-inline-end.rem]="0.625"></th>
          </tr>
        </thead>
        <tbody>
          @for (record of automatic(); track $index) {
            <tr>
              <td>{{ record.name }}</td>
              <td>{{ record.type }}</td>
              <td>{{ record.value }}</td>
              <td>{{ record.ttl }}</td>
              <td>{{ record.source }}</td>
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
                  No automatic DNS records. Devices you trust can add their own
                  via RFC 2136.
                </app-placeholder>
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
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    TuiCardLarge,
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
  protected readonly manual = computed(() =>
    (this.records() || []).filter(r => r.source === null),
  )
  protected readonly automatic = computed(() =>
    (this.records() || []).filter(r => r.source !== null),
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
