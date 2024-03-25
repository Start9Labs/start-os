import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, NavigationExtras, Router } from '@angular/router'
import { Manifest } from '@start9labs/marketplace'
import { isEmptyObject } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, switchMap } from 'rxjs'
import { ConnectionService } from 'src/app/services/connection.service'
import {
  DependencyErrorType,
  DepErrorService,
  PkgDependencyErrors,
} from 'src/app/services/dep-error.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  DataModel,
  HealthCheckResult,
  MainStatus,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryRendering,
  renderPkgStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import { DependentInfo } from 'src/app/types/dependent-info'
import { ServiceActionsComponent } from '../components/actions.component'
import { ServiceAdditionalComponent } from '../components/additional.component'
import { ServiceDependenciesComponent } from '../components/dependencies.component'
import { ServiceHealthChecksComponent } from '../components/health-checks.component'
import { ServiceInterfaceListComponent } from '../components/interface-list.component'
import { ServiceMenuComponent } from '../components/menu.component'
import { ServiceProgressComponent } from '../components/progress.component'
import { ServiceStatusComponent } from '../components/status.component'
import {
  PackageConfigData,
  ServiceConfigModal,
} from 'src/app/apps/portal/modals/config.component'
import { DependencyInfo } from '../types/dependency-info'
import { getManifest } from 'src/app/util/get-package-data'
import { InstallingProgressPipe } from 'src/app/apps/portal/routes/service/pipes/install-progress.pipe'

@Component({
  template: `
    @if (service$ | async; as service) {
      <h3 class="g-title">Status</h3>
      <service-status
        [connected]="!!(connected$ | async)"
        [installingInfo]="service.pkg.stateInfo.installingInfo"
        [rendering]="getRendering(service.status)"
        [sigtermTimeout]="
          service.pkg.status.main.status === 'stopping'
            ? service.pkg.status.main.timeout
            : null
        "
      />

      @if (
        service.pkg.stateInfo.state === 'installing' ||
        service.pkg.stateInfo.state === 'updating' ||
        service.pkg.stateInfo.state === 'restoring'
      ) {
        <p
          *ngFor="
            let phase of service.pkg.stateInfo.installingInfo.progress.phases
          "
          [progress]="phase.progress"
        >
          {{ phase.name }}
        </p>
      } @else {
        @if (
          service.pkg.stateInfo.state === 'installed' &&
          service.status.primary !== 'backing-up'
        ) {
          @if (connected$ | async) {
            <service-actions
              [pkg]="service.pkg"
              [dependencies]="service.dependencies"
            />
          }

          <service-interface-list
            [pkg]="service.pkg"
            [status]="service.status"
          />

          @if (
            service.status.primary === 'running' && (health$ | async);
            as checks
          ) {
            <service-health-checks [checks]="checks" />
          }

          @if (service.dependencies.length) {
            <service-dependencies [dependencies]="service.dependencies" />
          }

          <service-menu [pkg]="service.pkg" />
          <service-additional [pkg]="service.pkg" />
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    ServiceProgressComponent,
    ServiceStatusComponent,
    ServiceActionsComponent,
    ServiceInterfaceListComponent,
    ServiceHealthChecksComponent,
    ServiceDependenciesComponent,
    ServiceMenuComponent,
    ServiceAdditionalComponent,
    InstallingProgressPipe,
  ],
})
export class ServiceRoute {
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly pkgId$ = inject(ActivatedRoute).paramMap.pipe(
    map(params => params.get('pkgId')!),
  )
  private readonly depErrorService = inject(DepErrorService)
  private readonly router = inject(Router)
  private readonly formDialog = inject(FormDialogService)
  readonly connected$ = inject(ConnectionService).connected$

  readonly service$ = this.pkgId$.pipe(
    switchMap(pkgId =>
      combineLatest([
        this.patch.watch$('packageData', pkgId),
        this.depErrorService.getPkgDepErrors$(pkgId),
      ]),
    ),
    map(([pkg, depErrors]) => {
      return {
        pkg,
        dependencies: this.getDepInfo(pkg, depErrors),
        status: renderPkgStatus(pkg, depErrors),
      }
    }),
  )

  readonly health$ = this.pkgId$.pipe(
    switchMap(pkgId =>
      this.patch.watch$('packageData', pkgId, 'status', 'main'),
    ),
    map(toHealthCheck),
  )

  getRendering({ primary }: PackageStatus): StatusRendering {
    return PrimaryRendering[primary]
  }

  private getDepInfo(
    pkg: PackageDataEntry,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo[] {
    const manifest = getManifest(pkg)

    return Object.keys(pkg.currentDependencies)
      .filter(id => !!manifest.dependencies[id])
      .map(id => this.getDepValues(pkg, manifest, id, depErrors))
  }

  private getDepValues(
    pkg: PackageDataEntry,
    pkgManifest: Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo {
    const { errorText, fixText, fixAction } = this.getDepErrors(
      pkg,
      pkgManifest,
      depId,
      depErrors,
    )

    const depInfo = pkg.dependencyInfo[depId]

    return {
      id: depId,
      version: pkg.currentDependencies[depId].versionRange,
      title: depInfo?.title || depId,
      icon: depInfo?.icon || '',
      errorText: errorText
        ? `${errorText}. ${pkgManifest.title} will not work as expected.`
        : '',
      actionText: fixText || 'View',
      action:
        fixAction ||
        (() => {
          this.router.navigate([`portal`, `service`, depId])
        }),
    }
  }

  private getDepErrors(
    pkg: PackageDataEntry,
    pkgManifest: Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ) {
    const depError = depErrors[pkgManifest.id]

    let errorText: string | null = null
    let fixText: string | null = null
    let fixAction: (() => any) | null = null

    if (depError) {
      if (depError.type === DependencyErrorType.NotInstalled) {
        errorText = 'Not installed'
        fixText = 'Install'
        fixAction = () => this.fixDep(pkg, pkgManifest, 'install', depId)
      } else if (depError.type === DependencyErrorType.IncorrectVersion) {
        errorText = 'Incorrect version'
        fixText = 'Update'
        fixAction = () => this.fixDep(pkg, pkgManifest, 'update', depId)
      } else if (depError.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config not satisfied'
        fixText = 'Auto config'
        fixAction = () => this.fixDep(pkg, pkgManifest, 'configure', depId)
      } else if (depError.type === DependencyErrorType.NotRunning) {
        errorText = 'Not running'
        fixText = 'Start'
      } else if (depError.type === DependencyErrorType.HealthChecksFailed) {
        errorText = 'Required health check not passing'
      } else if (depError.type === DependencyErrorType.Transitive) {
        errorText = 'Dependency has a dependency issue'
      }
    }

    return {
      errorText,
      fixText,
      fixAction,
    }
  }

  async fixDep(
    pkg: PackageDataEntry,
    pkgManifest: Manifest,
    action: 'install' | 'update' | 'configure',
    depId: string,
  ): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(pkg, pkgManifest, depId)
      case 'configure':
        return this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
          label: `${pkg.dependencyInfo[depId].title} config`,
          data: {
            pkgId: depId,
            dependentInfo: pkgManifest,
          },
        })
    }
  }

  private async installDep(
    pkg: PackageDataEntry,
    manifest: Manifest,
    depId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: manifest.id,
      title: manifest.title,
      version: pkg.currentDependencies[depId].versionRange,
    }
    const navigationExtras: NavigationExtras = {
      state: { dependentInfo },
    }

    await this.router.navigate(['marketplace', depId], navigationExtras)
  }
}

function toHealthCheck(main: MainStatus): HealthCheckResult[] | null {
  return main.status !== 'running' || isEmptyObject(main.health)
    ? null
    : Object.values(main.health)
}
