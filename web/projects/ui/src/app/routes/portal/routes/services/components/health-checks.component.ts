import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { ServiceHealthCheckComponent } from 'src/app/routes/portal/routes/services/components/health-check.component'
import { ConnectionService } from 'src/app/services/connection.service'

@Component({
  standalone: true,
  selector: 'service-health-checks',
  template: `
    <header>Health Checks</header>
    @for (check of checks; track $index) {
      <service-health-check
        [check]="check"
        [connected]="!!(connected$ | async)"
      />
    } @empty {
      <blockquote>No health checks</blockquote>
    }
  `,
  styles: `
    blockquote {
      text-align: center;
      font: var(--tui-font-text-l);
      color: var(--tui-text-tertiary);
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [AsyncPipe, ServiceHealthCheckComponent],
})
export class ServiceHealthChecksComponent {
  @Input({ required: true })
  checks: readonly T.NamedHealthCheckResult[] = []

  readonly connected$ = inject(ConnectionService)
}
