import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiComparator, TuiTable } from '@taiga-ui/addon-table'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getInstalledPrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { TitleDirective } from 'src/app/services/title.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServiceComponent } from './service.component'
import { ServicesService } from './services.service'

@Component({
  standalone: true,
  template: `
    <ng-container *title>Services</ng-container>
    <table tuiTable class="g-table" [(sorter)]="sorter">
      <thead>
        <tr>
          <th [style.width.rem]="3"></th>
          <th tuiTh [requiredSort]="true" [sorter]="name">Name</th>
          <th tuiTh>Version</th>
          <th tuiTh [requiredSort]="true" [sorter]="uptime">Uptime</th>
          <th
            tuiTh
            [requiredSort]="true"
            [sorter]="status"
            [style.width.rem]="13"
          >
            Status
          </th>
          <th [style.width.rem]="8" [style.text-indent.rem]="1.5">Controls</th>
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
          <tr>
            <td colspan="6">
              {{ services() ? 'No services installed' : 'Loading...' }}
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      position: relative;
      font-size: 1rem;
      overflow: hidden;
    }

    :host-context(tui-root._mobile) {
      padding: 0;
    }
  `,
  host: { class: 'g-page' },
  imports: [ServiceComponent, ToManifestPipe, TuiTable, TitleDirective],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class DashboardComponent {
  readonly services = toSignal(inject(ServicesService))
  readonly errors = toSignal(inject(DepErrorService).depErrors$)

  readonly name: TuiComparator<PackageDataEntry> = (a, b) =>
    getManifest(b).title.toLowerCase() > getManifest(a).title.toLowerCase()
      ? -1
      : 1

  readonly status: TuiComparator<PackageDataEntry> = (a, b) =>
    getInstalledPrimaryStatus(b) > getInstalledPrimaryStatus(a) ? -1 : 1

  readonly uptime: TuiComparator<any> = (a, b) =>
    a.status.started || '' > a.status.started || '' ? -1 : 1

  sorter = this.name
}
