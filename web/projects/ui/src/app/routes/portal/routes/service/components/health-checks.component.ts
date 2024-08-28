import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { ServiceHealthCheckComponent } from 'src/app/routes/portal/routes/service/components/health-check.component'
import { ConnectionService } from 'src/app/services/connection.service'

@Component({
  selector: 'service-health-checks',
  template: `
    @for (check of checks; track $index) {
      <service-health-check
        class="g-action"
        [check]="check"
        [connected]="!!(connected$ | async)"
      />
    }

    @if (!checks.length) {
      No health checks
    }
  `,
  styles: ':host { display: block; min-height: var(--tui-height-s) }',
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [AsyncPipe, ServiceHealthCheckComponent],
})
export class ServiceHealthChecksComponent {
  @Input({ required: true })
  checks: readonly T.NamedHealthCheckResult[] = []

  readonly connected$ = inject(ConnectionService)
}
