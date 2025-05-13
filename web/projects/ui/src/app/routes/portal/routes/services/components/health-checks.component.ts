import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiTable } from '@taiga-ui/addon-table'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { ServiceHealthCheckComponent } from './health-check.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  selector: 'service-health-checks',
  template: `
    <header>{{ 'Health Checks' | i18n }}</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>{{ 'Name' | i18n }}</th>
          <th tuiTh>{{ 'Status' | i18n }}</th>
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
        {{ 'No health checks' | i18n }}
      </app-placeholder>
    }
  `,
  styles: `
    :host {
      min-height: 12rem;
      grid-column: span 3;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ServiceHealthCheckComponent,
    PlaceholderComponent,
    TuiTable,
    i18nPipe,
  ],
})
export class ServiceHealthChecksComponent {
  readonly checks = input.required<readonly T.NamedHealthCheckResult[]>()
}
