import { TuiIcon } from '@taiga-ui/core'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { ServiceComponent } from 'src/app/routes/portal/routes/dashboard/service.component'
import { ServicesService } from 'src/app/routes/portal/routes/dashboard/services.service'
import { DepErrorService } from 'src/app/services/dep-error.service'

@Component({
  standalone: true,
  template: `
    <h2>
      <tui-icon icon="@tui.layout-grid" />
      Services
    </h2>
    <div class="g-plaque"></div>
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
      clip-path: var(--clip-path);
      backdrop-filter: blur(1rem);
      font-size: 1rem;
      overflow: hidden;

      --clip-path: polygon(
        0 2rem,
        1.25rem 0,
        8.75rem 0,
        calc(10rem + 0.1em) calc(2rem - 0.1em),
        calc(100% - 1.25rem) 2rem,
        100% 4rem,
        100% calc(100% - 2rem),
        calc(100% - 1.25rem) 100%,
        1.25rem 100%,
        0 calc(100% - 2rem)
      );
    }

    h2 {
      height: 2rem;
      display: flex;
      align-items: center;
      gap: 0.5rem;
      margin: 0;
      padding: 0 2rem;
      font-weight: bold;
      font-size: 1rem;

      tui-icon {
        font-size: 1rem;
      }
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
      --clip-path: none !important;

      table {
        width: 100%;
        margin: 0;
      }

      thead,
      h2 {
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
