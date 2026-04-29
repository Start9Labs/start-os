import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { T } from '@start9labs/start-sdk'
import {
  PackageDataEntry,
  StateInfo,
} from 'src/app/services/patch-db/data-model'
import { ServiceComponent } from './service.component'
import { TuiComparator, TuiTable, TuiSortDirection } from '@taiga-ui/addon-table'
import { getInstalledPrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { ToManifestPipe } from '../../../pipes/to-manifest'
import { toSignal } from '@angular/core/rxjs-interop'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { i18nPipe } from '@start9labs/shared'
import { TuiSkeleton } from '@taiga-ui/kit'
import { PlaceholderComponent } from '../../../components/placeholder.component'
import { TuiButton } from '@taiga-ui/core'
import { RouterLink } from '@angular/router'
import { ServicesPreferencesService } from 'src/app/services/services-preferences.service'
import { TuiTableSortChange } from '@taiga-ui/addon-table'

@Component({
  selector: '[services]',
  template: `
    @if (services()?.length === 0) {
      <app-placeholder>
        <h1 [style.margin-bottom]="0">
          {{ 'Welcome to' | i18n }}
          <span>StartOS!</span>
        </h1>

        <p>
          {{
            'To get started, visit the Marketplace and download your first service'
              | i18n
          }}
        </p>

        <a
          style="margin: 1.5rem 0;"
          tuiButton
          size="m"
          iconStart="@tui.shopping-cart"
          routerLink="../marketplace"
        >
          {{ 'View Marketplace' | i18n }}
        </a>
      </app-placeholder>
    } @else {
      <table
        [sorter]="name"
        [direction]="currentDirection()"
        [appTable]="[null, 'Name', 'Status', 'Version', 'Uptime']"
        [appTableSorters]="[null, name, status]"
        (sortChange)="onSortChange($event)"
      >
        @for (service of services() | tuiTableSort; track $index) {
          <tr
            appService
            [routerLink]="'/services/' + (service | toManifest)?.id"
            [pkg]="service"
            [depErrors]="errors()?.[(service | toManifest).id] || {}"
          ></tr>
        } @empty {
          @for (_ of ['', '']; track $index) {
            <tr>
              <td colspan="5">
                <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
              </td>
            </tr>
          }
        }
      </table>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    FormsModule,
    TableComponent,
    ServiceComponent,
    ToManifestPipe,
    i18nPipe,
    TuiSkeleton,
    PlaceholderComponent,
    TuiButton,
    RouterLink,
    TuiTable,
  ],
})
export class ServicesTableComponent<
  T extends T.PackageDataEntry & {
    stateInfo: StateInfo
  },
> {
  private readonly prefs = inject(ServicesPreferencesService)

  readonly errors = toSignal(inject(DepErrorService).depErrors$)

  readonly services = input.required<readonly T[] | null>()

  readonly currentDirection = computed<TuiSortDirection>(() => {
    return this.prefs.sortState$.value.direction as TuiSortDirection
  })

  readonly name: TuiComparator<PackageDataEntry> = byName

  readonly status: TuiComparator<PackageDataEntry> = (a, b) =>
    getInstalledPrimaryStatus(b) > getInstalledPrimaryStatus(a) ? -1 : 1

  onSortChange(event: TuiTableSortChange<PackageDataEntry>) {
    const column = event.sortComparator === this.name ? 'name' : 'status'
    this.prefs.setSort(column, event.sortDirection)
  }
}

function byName(a: PackageDataEntry, b: PackageDataEntry) {
  return getManifest(b).title.toLowerCase() > getManifest(a).title.toLowerCase()
    ? -1
    : 1
}
