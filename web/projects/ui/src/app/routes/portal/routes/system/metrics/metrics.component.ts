import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { MetricsComponent } from 'src/app/routes/portal/routes/dashboard/metrics.component'
import { MetricsService } from 'src/app/services/metrics.service'

@Component({
  template: `
    <app-metrics [metrics]="metrics$ | async" />
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [MetricsComponent, AsyncPipe],
})
export default class SystemMetricsComponent {
  readonly metrics$ = inject(MetricsService)
}
