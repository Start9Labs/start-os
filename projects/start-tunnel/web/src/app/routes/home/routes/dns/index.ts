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
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TUI_CONFIRM,
  TuiNotificationMiddleService,
  TuiSkeleton,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
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
      <header tuiHeader="body-l">
        <tui-icon icon="@tui.pencil" />
        <h3 tuiTitle>Manual</h3>
        <aside tuiAccessories>
          <button tuiButton iconStart="@tui.plus" (click)="onAdd()">Add</button>
        </aside>
      </header>
      <table class="g-table" [tuiSkeleton]="!records()">
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Server</th>
            <th>TTL</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          @for (record of manual(); track $index) {
            <tr>
              <td>{{ record.name }}</td>
              <td>{{ record.type }}</td>
              <td>{{ serverDisplay(record) }}</td>
              <td>{{ record.ttl }}</td>
              <td [style.padding-inline-end.rem]="0.625">
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
      <header tuiHeader="body-l">
        <tui-icon icon="@tui.zap" />
        <h3 tuiTitle>Automatic</h3>
      </header>
      <table class="g-table no-actions" [tuiSkeleton]="!records()">
        <thead>
          <tr>
            <th>Name</th>
            <th>Type</th>
            <th>Server</th>
            <th>TTL</th>
          </tr>
        </thead>
        <tbody>
          @for (record of automatic(); track $index) {
            <tr>
              <td>{{ record.name }}</td>
              <td>{{ record.type }}</td>
              <td>{{ serverDisplay(record) }}</td>
              <td>{{ record.ttl }}</td>
            </tr>
          } @empty {
            <tr>
              <td colspan="4">
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
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    TuiCardLarge,
    TuiDropdown,
    TuiDataList,
    PlaceholderComponent,
    TuiSkeleton,
    TuiHeader,
    TuiIcon,
    TuiTitle,
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

  // DNS records point a name at a server, so the picker lists servers only.
  protected readonly servers: Signal<MappedDevice[]> = toSignal(
    this.patch.watch$('wg', 'subnets').pipe(
      map(subnets =>
        Object.values(subnets).flatMap(({ clients }) =>
          Object.entries(clients)
            .filter(([, c]) => c.kind === 'server')
            .map(([ip, { name }]) => ({ ip, name })),
        ),
      ),
    ),
    { initialValue: [] },
  )

  // Only A/AAAA values are server IPs; for those, show the server's friendly
  // name and IP (the injecting server for automatic records, the selected one
  // for manual). CNAME/TXT/other rdata renders verbatim.
  protected serverDisplay(record: {
    type: string
    source: string | null
    value: string
  }): string {
    if (record.type !== 'A' && record.type !== 'AAAA') return record.value

    const ip = record.source ?? record.value
    const name = this.devices().find(d => d.ip === ip)?.name
    return name ? `${name} (${ip})` : record.value
  }

  protected onAdd(): void {
    this.dialogs
      .open(DNS_ADD, {
        label: 'Add DNS record',
        data: { devices: this.servers },
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
