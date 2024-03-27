import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ServiceComponent } from 'src/app/apps/portal/routes/dashboard/service.component'
import { ServicesService } from 'src/app/apps/portal/services/services.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { ToManifestPipe } from '../../pipes/to-manifest'

@Component({
  standalone: true,
  selector: 'app-services',
  template: `
    <ng-content />
    <table>
      <thead>
        <tr>
          <th [style.width.rem]="3"></th>
          <th>Name</th>
          <th>Version</th>
          <th [style.width.rem]="13">Status</th>
          <th [style.width.rem]="8" [style.text-align]="'center'">Controls</th>
        </tr>
      </thead>
      <tbody>
        @if (errors$ | async; as errors) {
          @for (pkg of services$ | async; track $index) {
            <tr
              appService
              [pkg]="pkg"
              [depErrors]="errors[(pkg | toManifest).id]"
            ></tr>
          } @empty {
            <tr>
              <td colspan="5">No services installed</td>
            </tr>
          }
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      grid-column: 1/4;
      margin-top: -2rem;

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

    table {
      width: calc(100% - 4rem);
      margin: 2rem;
    }

    tr:not(:last-child) {
      box-shadow: inset 0 -1px var(--tui-clear);
    }

    th {
      text-transform: uppercase;
      color: var(--tui-text-02);
      font: var(--tui-font-text-s);
      font-weight: bold;
      text-align: left;
      padding: 0 0.5rem;
    }

    td {
      text-align: center;
      padding: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ServiceComponent, AsyncPipe, ToManifestPipe],
})
export class ServicesComponent {
  readonly services$ = inject(ServicesService)
  readonly errors$ = inject(DepErrorService).depErrors$
}
