import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, NavigationExtras, Router } from '@angular/router'
import { Manifest } from '@start9labs/marketplace'
import { getPkgId, isEmptyObject } from '@start9labs/shared'
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
  InstalledPackageInfo,
  MainStatus,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryRendering,
  PrimaryStatus,
  renderPkgStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import { DependentInfo } from 'src/app/types/dependent-info'
import { ServiceActionsComponent } from '../components/actions.component'
import { ServiceAdditionalComponent } from '../components/additional.component'
import { ServiceDependenciesComponent } from '../components/dependencies.component'
import { ServiceHealthChecksComponent } from '../components/health-checks.component'
import { ServiceInterfacesComponent } from '../components/interfaces.component'
import { ServiceMenuComponent } from '../components/menu.component'
import { ServiceProgressComponent } from '../components/progress.component'
import { ServiceStatusComponent } from '../components/status.component'
import {
  PackageConfigData,
  ServiceConfigModal,
} from 'src/app/apps/portal/modals/config.component'
import { ProgressDataPipe } from '../pipes/progress-data.pipe'
import { DependencyInfo } from '../types/dependency-info'

const STATES = [
  PackageState.Installing,
  PackageState.Updating,
  PackageState.Restoring,
]

@Component({
  template: `
    @if (service$ | async; as service) {
      @if (showProgress(service.pkg)) {
        @if (service.pkg | progressData; as progress) {
          <p [progress]="progress.downloadProgress">Downloading</p>
          <p [progress]="progress.validateProgress">Validating</p>
          <p [progress]="progress.unpackProgress">Unpacking</p>
        }
      } @else {
        <h3 class="g-title">Status</h3>
        <service-status
          [connected]="!!(connected$ | async)"
          [installProgress]="service.pkg['install-progress']"
          [rendering]="$any(getRendering(service.status))"
        />

        @if (isInstalled(service.pkg) && (connected$ | async)) {
          <service-actions
            [service]="service.pkg"
            [dependencies]="service.dependencies"
          />
        }

        @if (isInstalled(service.pkg) && !isBackingUp(service.status)) {
          <service-interfaces [service]="service" />

          @if (isRunning(service.status) && (health$ | async); as checks) {
            <service-health-checks [checks]="checks" />
          }

          @if (service.dependencies.length) {
            <service-dependencies [dependencies]="service.dependencies" />
          }

          <service-menu [service]="service.pkg" />
          <service-additional [service]="service.pkg" />
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
    ServiceInterfacesComponent,
    ServiceHealthChecksComponent,
    ServiceDependenciesComponent,
    ServiceMenuComponent,
    ServiceAdditionalComponent,

    ProgressDataPipe,
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
        this.patch.watch$('package-data', pkgId),
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
      this.patch.watch$('package-data', pkgId, 'installed', 'status', 'main'),
    ),
    map(toHealthCheck),
  )

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

  private getDepInfo(
    { installed, manifest }: PackageDataEntry,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo[] {
    return installed
      ? Object.keys(installed['current-dependencies'])
          .filter(depId => !!manifest.dependencies[depId])
          .map(depId =>
            this.getDepValues(installed, manifest, depId, depErrors),
          )
      : []
  }

  private getDepValues(
    pkgInstalled: InstalledPackageInfo,
    pkgManifest: Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo {
    const { errorText, fixText, fixAction } = this.getDepErrors(
      pkgInstalled,
      pkgManifest,
      depId,
      depErrors,
    )

    const depInfo = pkgInstalled['dependency-info'][depId]

    return {
      id: depId,
      version: pkgManifest.dependencies[depId].version, // do we want this version range?
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
    pkgInstalled: InstalledPackageInfo,
    pkgManifest: Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ) {
    const depError = (depErrors[pkgManifest.id] as any)?.[depId] // @TODO fix

    let errorText: string | null = null
    let fixText: string | null = null
    let fixAction: (() => any) | null = null

    if (depError) {
      if (depError.type === DependencyErrorType.NotInstalled) {
        errorText = 'Not installed'
        fixText = 'Install'
        fixAction = () =>
          this.fixDep(pkgInstalled, pkgManifest, 'install', depId)
      } else if (depError.type === DependencyErrorType.IncorrectVersion) {
        errorText = 'Incorrect version'
        fixText = 'Update'
        fixAction = () =>
          this.fixDep(pkgInstalled, pkgManifest, 'update', depId)
      } else if (depError.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config not satisfied'
        fixText = 'Auto config'
        fixAction = () =>
          this.fixDep(pkgInstalled, pkgManifest, 'configure', depId)
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
    pkgInstalled: InstalledPackageInfo,
    pkgManifest: Manifest,
    action: 'install' | 'update' | 'configure',
    depId: string,
  ): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(pkgManifest, depId)
      case 'configure':
        return this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
          label: `${pkgInstalled!['dependency-info'][depId].title} config`,
          data: {
            pkgId: depId,
            dependentInfo: pkgManifest,
          },
        })
    }
  }

  private async installDep(manifest: Manifest, depId: string): Promise<void> {
    const version = manifest.dependencies[depId].version

    const dependentInfo: DependentInfo = {
      id: manifest.id,
      title: manifest.title,
      version,
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
