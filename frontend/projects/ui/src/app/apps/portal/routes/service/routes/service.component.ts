import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId, isEmptyObject } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import {
  DataModel,
  HealthCheckResult,
  MainStatus,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryRendering,
  PrimaryStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { ServiceProgressComponent } from '../components/progress.component'
import { ServiceStatusComponent } from '../components/status.component'
import { ServiceActionsComponent } from '../components/actions.component'
import { ServiceInterfacesComponent } from '../components/interfaces.component'
import { ServiceHealthChecksComponent } from '../components/health-checks.component'
import { ServiceDependenciesComponent } from '../components/dependencies.component'
import { ServiceMenuComponent } from '../components/menu.component'
import { ServiceAdditionalComponent } from '../components/additional.component'
import { ProgressDataPipe } from '../pipes/progress-data.pipe'
import { ToDependenciesPipe } from '../pipes/to-dependencies.pipe'
import { ToStatusPipe } from '../pipes/to-status.pipe'

const STATES = [
  PackageState.Installing,
  PackageState.Updating,
  PackageState.Restoring,
]

@Component({
  template: `
    <ng-container *ngIf="service$ | async as service">
      <ng-container *ngIf="showProgress(service); else installed">
        <ng-container *ngIf="service | progressData as progress">
          <p [progress]="progress.downloadProgress">Downloading</p>
          <p [progress]="progress.validateProgress">Validating</p>
          <p [progress]="progress.unpackProgress">Unpacking</p>
        </ng-container>
      </ng-container>

      <ng-template #installed>
        <ng-container *ngIf="service | toStatus as status">
          <h3 class="g-title">Status</h3>
          <service-status
            [connected]="!!(connected$ | async)"
            [installProgress]="service['install-progress']"
            [rendering]="$any(getRendering(status))"
          />
          <service-actions
            *ngIf="isInstalled(service) && (connected$ | async)"
            [service]="service"
          />

          <ng-container *ngIf="isInstalled(service) && !isBackingUp(status)">
            <service-interfaces [service]="service" />
            <service-health-checks
              *ngIf="isRunning(status) && (health$ | async) as checks"
              [checks]="checks"
            />
            <service-dependencies
              *ngIf="service | toDependencies as dependencies"
              [dependencies]="dependencies"
            />
            <service-menu [service]="service" />
            <service-additional [service]="service" />
          </ng-container>
        </ng-container>
      </ng-template>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,

    ServiceProgressComponent,
    ServiceStatusComponent,
    ServiceActionsComponent,
    ServiceInterfacesComponent,
    ServiceHealthChecksComponent,
    ServiceDependenciesComponent,
    ServiceMenuComponent,
    ServiceAdditionalComponent,

    ProgressDataPipe,
    ToDependenciesPipe,
    ToStatusPipe,
  ],
})
export class ServiceRoute {
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly pkgId = getPkgId(inject(ActivatedRoute))

  readonly connected$ = inject(ConnectionService).connected$
  readonly service$ = this.patch.watch$('package-data', this.pkgId)
  readonly health$ = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'status', 'main')
    .pipe(map(toHealthCheck))

  getRendering({ primary }: PackageStatus): StatusRendering {
    return PrimaryRendering[primary]
  }

  isInstalled({ state }: PackageDataEntry): boolean {
    return state === PackageState.Installed
  }

  isRunning({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.Running
  }

  isBackingUp({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.BackingUp
  }

  showProgress({ state }: PackageDataEntry): boolean {
    return STATES.includes(state)
  }
}

function toHealthCheck(main: MainStatus): HealthCheckResult[] | null {
  return main.status !== 'running' || isEmptyObject(main.health)
    ? null
    : Object.values(main.health)
}
