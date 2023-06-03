import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import { DataModel, HealthResult } from 'src/app/services/patch-db/data-model'
import { isEmptyObject } from '@start9labs/shared'

@Component({
  selector: 'app-show-health-checks',
  templateUrl: './app-show-health-checks.component.html',
  styleUrls: ['./app-show-health-checks.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowHealthChecksComponent {
  @Input() pkgId!: string

  readonly connected$ = this.connectionService.connected$

  get healthChecks$() {
    return this.patch
      .watch$('package-data', this.pkgId, 'installed', 'status', 'main')
      .pipe(
        map(main => {
          if (main.status !== 'running' || isEmptyObject(main.health))
            return null
          return Object.values(main.health)
        }),
      )
  }

  constructor(
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

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
