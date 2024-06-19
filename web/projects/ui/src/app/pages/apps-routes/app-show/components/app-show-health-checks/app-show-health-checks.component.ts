import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { ConnectionService } from 'src/app/services/connection.service'

@Component({
  selector: 'app-show-health-checks',
  templateUrl: './app-show-health-checks.component.html',
  styleUrls: ['./app-show-health-checks.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowHealthChecksComponent {
  @Input()
  healthChecks!: Record<string, T.HealthCheckResult>

  constructor(readonly connection$: ConnectionService) {}

  isLoading(result: T.HealthCheckResult['result']): boolean {
    return result === 'starting' || result === 'loading'
  }

  isReady(result: T.HealthCheckResult['result']): boolean {
    return result !== 'failure' && result !== 'loading'
  }

  asIsOrder() {
    return 0
  }
}
