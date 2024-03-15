import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ConnectionService } from 'src/app/services/connection.service'
import { HealthResult, Manifest } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-health-checks',
  templateUrl: './app-show-health-checks.component.html',
  styleUrls: ['./app-show-health-checks.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowHealthChecksComponent {
  @Input()
  manifest!: Manifest

  HealthResult = HealthResult

  readonly connected$ = this.connectionService.connected$

  constructor(private readonly connectionService: ConnectionService) {}

  isLoading(result: HealthResult): boolean {
    return result === HealthResult.Starting || result === HealthResult.Loading
  }

  isReady(result: HealthResult): boolean {
    return result !== HealthResult.Failure && result !== HealthResult.Loading
  }

  asIsOrder() {
    return 0
  }
}
