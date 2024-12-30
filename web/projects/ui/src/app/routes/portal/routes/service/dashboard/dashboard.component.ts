import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiIcon } from '@taiga-ui/core'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { ServiceComponent } from './service.component'
import { ServicesService } from './services.service'

@Component({
  standalone: true,
  template: `
    <table>
      <thead>
        <tr>
          <th [style.width.rem]="3"></th>
          <th>Name</th>
          <th>Version</th>
          <th [style.width.rem]="13">Status</th>
          <th [style.width.rem]="8" [style.text-indent.rem]="1.5">Controls</th>
        </tr>
      </thead>
      <tbody>
        @for (pkg of services(); track $index) {
          <tr
            appService
            [pkg]="pkg"
            [depErrors]="errors()?.[(pkg | toManifest).id]"
          ></tr>
        } @empty {
          <tr>
            <td colspan="5">
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
      max-width: 64rem;
      margin: 0 auto;
      font-size: 1rem;
      overflow: hidden;
    }

    table {
      width: calc(100% - 4rem);
      margin: 2rem;
    }

    tr:not(:last-child) {
      box-shadow: inset 0 -1px var(--tui-background-neutral-1);
    }

    th {
      text-transform: uppercase;
      color: var(--tui-text-secondary);
      font: var(--tui-font-text-s);
      font-weight: bold;
      text-align: left;
      padding: 0 0.5rem;
    }

    td {
      text-align: center;
      padding: 1rem;
    }

    :host-context(tui-root._mobile) {
      height: calc(100vh - 7.375rem);

      table {
        width: 100%;
        margin: 0;
      }

      thead {
        display: none;
      }
    }
  `,
  imports: [TuiIcon, ServiceComponent, ToManifestPipe],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DashboardComponent {
  readonly services = toSignal(inject(ServicesService))
  readonly errors = toSignal(inject(DepErrorService).depErrors$)
}
