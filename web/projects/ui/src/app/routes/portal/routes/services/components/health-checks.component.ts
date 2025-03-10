import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiTable } from '@taiga-ui/addon-table'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { ServiceHealthCheckComponent } from './health-check.component'

@Component({
  standalone: true,
  selector: 'service-health-checks',
  template: `
    <header>Health Checks</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>Name</th>
          <th tuiTh>Status</th>
        </tr>
      </thead>
      <tbody>
        @for (check of checks(); track $index) {
          <tr [healthCheck]="check"></tr>
        }
      </tbody>
    </table>
    @if (!checks().length) {
      <app-placeholder icon="@tui.heart-pulse">
        No health checks
      </app-placeholder>
    }
  `,
  styles: `
    :host {
      grid-column: span 3;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ServiceHealthCheckComponent, PlaceholderComponent, TuiTable],
})
export class ServiceHealthChecksComponent {
  readonly checks = input.required<readonly T.NamedHealthCheckResult[]>()
}
