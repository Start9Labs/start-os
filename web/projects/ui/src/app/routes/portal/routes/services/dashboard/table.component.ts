import {
  ChangeDetectionStrategy,
  Component,
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
import { TuiComparator } from '@taiga-ui/addon-table'
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

@Component({
  selector: '[services]',
  template: `
    <!-- <table tuiTable [(sorter)]="sorter">
      <thead>
        <tr>
          <th [style.width.rem]="3"></th>
          <th tuiTh [requiredSort]="true" [sorter]="name">
            {{ 'Name' | i18n }}
          </th>
          <th tuiTh [requiredSort]="true" [sorter]="status">
            {{ 'Status' | i18n }}
          </th>
          <th tuiTh>{{ 'Version' | i18n }}</th>
          <th tuiTh [style.width.rem]="10">
            {{ 'Uptime' | i18n }}
          </th>
        </tr>
      </thead>
      <tbody>
        @for (pkg of services() | tuiTableSort; track $index) {
          <tr
            appService
            [pkg]="pkg"
            [depErrors]="errors()?.[(pkg | toManifest).id]"
          ></tr>
        } @empty {
          @if (services()) {
            <app-placeholder>
              <h1 [style.marginBottom]="0">
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
            @for (_ of ['', '']; track $index) {
              <tr>
                <td colspan="5">
                  <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
                </td>
              </tr>
            }
          }
        }
      </tbody>
    </table> -->
    @if (services()?.length === 0) {
      <app-placeholder>
        <h1 [style.marginBottom]="0">
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
      <table [appTable]="[null, 'Name', 'Status', 'Version', 'Uptime']">
        @for (service of services(); track service) {
          <tr
            appService
            [pkg]="service"
            [depErrors]="errors()?.[(service | toManifest).id]"
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
  ],
})
export class ServicesTableComponent<
  T extends T.PackageDataEntry & {
    stateInfo: StateInfo
  },
> {
  readonly errors = toSignal(inject(DepErrorService).depErrors$)

  readonly services = input.required<readonly T[] | null>()

  readonly name: TuiComparator<PackageDataEntry> = byName

  readonly status: TuiComparator<PackageDataEntry> = (a, b) =>
    getInstalledPrimaryStatus(b) > getInstalledPrimaryStatus(a) ? -1 : 1

  sorter = this.name
}

function byName(a: PackageDataEntry, b: PackageDataEntry) {
  return getManifest(b).title.toLowerCase() > getManifest(a).title.toLowerCase()
    ? -1
    : 1
}
