import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ConnectionService } from 'src/app/services/connection.service'
import { HealthCheckResult } from '../../../../../../../../../../core/startos/bindings/HealthCheckResult'

@Component({
  selector: 'app-show-health-checks',
  templateUrl: './app-show-health-checks.component.html',
  styleUrls: ['./app-show-health-checks.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowHealthChecksComponent {
  @Input()
  healthChecks!: Record<string, HealthCheckResult>

  readonly connected$ = this.connectionService.connected$

  constructor(private readonly connectionService: ConnectionService) {}

  isLoading(result: HealthCheckResult['result']): boolean {
    return result === 'starting' || result === 'loading'
  }

  isReady(result: HealthCheckResult['result']): boolean {
    return result !== 'failure' && result !== 'loading'
  }

  asIsOrder() {
    return 0
  }
}
