import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, NavigationExtras, Router } from '@angular/router'
import { isEmptyObject } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, switchMap } from 'rxjs'
import {
  ConfigModal,
  PackageConfigData,
} from 'src/app/routes/portal/modals/config.component'
import { ServiceBackupsComponent } from 'src/app/routes/portal/routes/service/components/backups.component'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/service/pipes/install-progress.pipe'
import { ConnectionService } from 'src/app/services/connection.service'
import {
  DepErrorService,
  PkgDependencyErrors,
} from 'src/app/services/dep-error.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryRendering,
  renderPkgStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import { DependentInfo } from 'src/app/types/dependent-info'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServiceActionsComponent } from '../components/actions.component'
import { ServiceDependenciesComponent } from '../components/dependencies.component'
import { ServiceHealthChecksComponent } from '../components/health-checks.component'
import { ServiceInterfaceListComponent } from '../components/interface-list.component'
import { ServiceMenuComponent } from '../components/menu.component'
import { ServiceProgressComponent } from '../components/progress.component'
import { ServiceStatusComponent } from '../components/status.component'
import { DependencyInfo } from '../types/dependency-info'

@Component({
  template: `
    @if (service$ | async; as service) {
      <section [style.grid-column]="'span 3'">
        <h3>Status</h3>
        <service-status
          [connected]="!!(connected$ | async)"
          [installingInfo]="service.pkg.stateInfo.installingInfo"
          [rendering]="getRendering(service.status)"
        />

        @if (isInstalled(service) && (connected$ | async)) {
          <service-actions
            [pkg]="service.pkg"
            [dependencies]="service.dependencies"
          />
        }
      </section>

      @if (isInstalled(service)) {
        <section [style.grid-column]="'span 3'">
          <h3>Backups</h3>
          <service-backups [pkg]="service.pkg" />
        </section>

        <section [style.grid-column]="'span 6'">
          <h3>Metrics</h3>
          TODO
        </section>

        <section [style.grid-column]="'span 4'" [style.align-self]="'start'">
          <h3>Menu</h3>
          <service-menu [pkg]="service.pkg" />
        </section>

        <div>
          <section>
            <h3>Health Checks</h3>
            <service-health-checks [checks]="(health$ | async) || []" />
          </section>

          <section>
            <h3>Dependencies</h3>
            <service-dependencies [dependencies]="service.dependencies" />
          </section>
        </div>

        <section [style.grid-column]="'span 4'" [style.align-self]="'start'">
          <h3>Service Interfaces</h3>
          <service-interface-list
            [pkg]="service.pkg"
            [status]="service.status"
          />
        </section>
      }

      @if (isInstalling(service.pkg.stateInfo.state)) {
        @for (
          item of service.pkg.stateInfo.installingInfo?.progress?.phases;
          track $index
        ) {
          <p [progress]="item.progress">{{ item.name }}</p>
        }
      }
    }
  `,
  styles: `
    :host {
      display: grid;
      grid-template-columns: repeat(12, 1fr);
      flex-direction: column;
      gap: 1rem;
      margin: 1rem -1rem 0;
    }

    :host-context(tui-root._mobile) {
      display: flex;
    }

    section {
      display: flex;
      flex-direction: column;
      width: 100%;
      padding: 1rem 1.5rem 0.5rem;
      border-radius: 0.5rem;
      background: var(--tui-background-neutral-1);
      box-shadow: inset 0 7rem 0 -4rem var(--tui-background-neutral-1);
      clip-path: polygon(0 1.5rem, 1.5rem 0, 100% 0, 100% 100%, 0 100%);
    }

    h3 {
      margin-bottom: 1.25rem;
    }

    div {
      display: flex;
      flex-direction: column;
      gap: inherit;
      grid-column: span 4;
    }

    :host-context(tui-root._mobile) {
      margin: 0;
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
    ServiceBackupsComponent,
    InstallingProgressPipe,
  ],
})
export class ServiceRoute {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly pkgId$ = inject(ActivatedRoute).paramMap.pipe(
    map(params => params.get('pkgId')!),
  )
  private readonly depErrorService = inject(DepErrorService)
  private readonly router = inject(Router)
  private readonly formDialog = inject(FormDialogService)
  readonly connected$ = inject(ConnectionService)

  readonly service$ = this.pkgId$.pipe(
    switchMap(pkgId =>
      combineLatest([
        this.patch.watch$('packageData', pkgId),
        this.depErrorService.getPkgDepErrors$(pkgId),
      ]),
    ),
    map(([pkg, depErrors]) => ({
      pkg,
      dependencies: this.getDepInfo(pkg, depErrors),
      status: renderPkgStatus(pkg, depErrors),
    })),
  )

  readonly health$ = this.pkgId$.pipe(
    switchMap(pkgId =>
      this.patch.watch$('packageData', pkgId, 'status', 'main'),
    ),
    map(toHealthCheck),
  )

  isInstalling(state: string): boolean {
    return (
      state === 'installing' || state === 'updating' || state === 'restoring'
    )
  }

  isInstalled({ pkg, status }: any): boolean {
    return pkg.stateInfo.state === 'installed' && status.primary !== 'backingUp'
  }

  getRendering({ primary }: PackageStatus): StatusRendering {
    return PrimaryRendering[primary]
  }

  private getDepInfo(
    pkg: PackageDataEntry,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo[] {
    const manifest = getManifest(pkg)

    return Object.keys(pkg.currentDependencies).map(id =>
      this.getDepValues(pkg, manifest, id, depErrors),
    )
  }

  private getDepValues(
    pkg: PackageDataEntry,
    pkgManifest: T.Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo {
    const { errorText, fixText, fixAction } = this.getDepErrors(
      pkg,
      pkgManifest,
      depId,
      depErrors,
    )

    const { title, icon, versionRange } = pkg.currentDependencies[depId]

    return {
      id: depId,
      version: versionRange,
      title,
      icon,
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
    pkgManifest: T.Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ) {
    const depError = depErrors[depId]

    let errorText: string | null = null
    let fixText: string | null = null
    let fixAction: (() => any) | null = null

    if (depError) {
      if (depError.type === 'notInstalled') {
        errorText = 'Not installed'
        fixText = 'Install'
        fixAction = () => this.fixDep(pkg, pkgManifest, 'install', depId)
      } else if (depError.type === 'incorrectVersion') {
        errorText = 'Incorrect version'
        fixText = 'Update'
        fixAction = () => this.fixDep(pkg, pkgManifest, 'update', depId)
      } else if (depError.type === 'configUnsatisfied') {
        errorText = 'Config not satisfied'
        fixText = 'Auto config'
        fixAction = () => this.fixDep(pkg, pkgManifest, 'configure', depId)
      } else if (depError.type === 'notRunning') {
        errorText = 'Not running'
        fixText = 'Start'
      } else if (depError.type === 'healthChecksFailed') {
        errorText = 'Required health check not passing'
      } else if (depError.type === 'transitive') {
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
    pkgManifest: T.Manifest,
    action: 'install' | 'update' | 'configure',
    depId: string,
  ): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(pkg, pkgManifest, depId)
      case 'configure':
        return this.formDialog.open<PackageConfigData>(ConfigModal, {
          label: `${pkg.currentDependencies[depId].title} config`,
          data: {
            pkgId: depId,
            dependentInfo: pkgManifest,
          },
        })
    }
  }

  private async installDep(
    pkg: PackageDataEntry,
    manifest: T.Manifest,
    depId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: manifest.id,
      title: manifest.title,
      version: pkg.currentDependencies[depId].versionRange,
    }
    const navigationExtras: NavigationExtras = {
      // @TODO state not being used by marketplace component. Maybe it is not important to use.
      state: { dependentInfo },
      queryParams: { id: depId },
    }

    await this.router.navigate(
      ['portal', 'system', 'marketplace'],
      navigationExtras,
    )
  }
}

function toHealthCheck(main: T.MainStatus): T.HealthCheckResult[] | null {
  return main.status !== 'running' || isEmptyObject(main.health)
    ? null
    : Object.values(main.health)
}
