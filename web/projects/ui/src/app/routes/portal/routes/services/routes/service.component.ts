import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, NavigationExtras, Router } from '@angular/router'
import { isEmptyObject } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map, switchMap } from 'rxjs'
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
import { ServiceActionRequestComponent } from '../components/action-request.component'
import { ServiceActionsComponent } from '../components/actions.component'
import { ServiceDependenciesComponent } from '../components/dependencies.component'
import { ServiceErrorComponent } from '../components/error.component'
import { ServiceHealthChecksComponent } from '../components/health-checks.component'
import { ServiceInterfaceListComponent } from '../components/interface-list.component'
import { ServiceProgressComponent } from '../components/progress.component'
import { ServiceStatusComponent } from '../components/status.component'
import { ToActionRequestsPipe } from '../pipes/to-action-requests.pipe'
import { DependencyInfo } from '../types/dependency-info'

@Component({
  template: `
    @if (service$ | async; as service) {
      <section class="g-card">
        <header>Status</header>
        <service-status
          [connected]="!!(connected$ | async)"
          [installingInfo]="service.pkg.stateInfo.installingInfo"
          [rendering]="getRendering(service.status)"
        />

        @if (isInstalled(service) && (connected$ | async)) {
          <service-actions [service]="service" />
        }
      </section>

      @if (isInstalled(service)) {
        @if (service.pkg.status.main === 'error') {
          <section class="error">
            <header>Error</header>
            <service-error [pkg]="service.pkg" />
          </section>
        } @else {
          <section class="g-card" [style.grid-column]="'span 2'">
            <header>Metrics</header>
            TODO
          </section>
        }

        <section class="g-card" [style.grid-column]="'span 3'">
          <header>Interfaces</header>
          <service-interface-list
            [pkg]="service.pkg"
            [status]="service.status"
          />
        </section>

        <section class="g-card">
          <header>Dependencies</header>
          <service-dependencies [dependencies]="service.dependencies" />
        </section>

        <section class="g-card">
          <header>Health Checks</header>
          <service-health-checks [checks]="(health$ | async) || []" />
        </section>

        @if (service.pkg | toActionRequests: service.allPkgs; as requests) {
          <section class="g-card">
            <header>Tasks</header>
            @for (request of requests.critical; track $index) {
              <button
                [actionRequest]="request"
                [pkg]="service.pkg"
                [allPkgs]="service.allPkgs"
              ></button>
            }
            @for (request of requests.important; track $index) {
              <button
                [actionRequest]="request"
                [pkg]="service.pkg"
                [allPkgs]="service.allPkgs"
              ></button>
            }
          </section>
        }
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
      grid-template-columns: repeat(3, 1fr);
      grid-auto-rows: max-content;
      gap: 1rem;
    }

    //section {
    //  display: flex;
    //  flex-direction: column;
    //  width: 100%;
    //  padding: 1rem 1.5rem 0.5rem;
    //  border-radius: 0.5rem;
    //  background: var(--tui-background-neutral-1);
    //  box-shadow: inset 0 7rem 0 -4rem var(--tui-background-neutral-1);
    //  clip-path: polygon(0 1.5rem, 1.5rem 0, 100% 0, 100% 100%, 0 100%);
    //
    //  &.error {
    //    box-shadow: inset 0 7rem 0 -4rem var(--tui-status-negative-pale);
    //    grid-column: span 6;
    //
    //    h3 {
    //      color: var(--tui-status-negative);
    //    }
    //  }
    //
    //  ::ng-deep [tuiCell] {
    //    width: stretch;
    //    margin: 0 -1rem;
    //
    //    &:not(:last-child) {
    //      box-shadow: 0 0.51rem 0 -0.5rem;
    //    }
    //  }
    //}
  `,
  host: { class: 'g-subpage' },
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
    ServiceActionRequestComponent,
    ServiceErrorComponent,
    ToActionRequestsPipe,
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
        this.patch.watch$('packageData'),
        this.depErrorService.getPkgDepErrors$(pkgId),
      ]).pipe(
        map(([allPkgs, depErrors]) => {
          const pkg = allPkgs[pkgId]
          return {
            allPkgs,
            pkg,
            dependencies: this.getDepInfo(pkg, depErrors),
            status: renderPkgStatus(pkg, depErrors),
          }
        }),
      ),
    ),
  )

  readonly health$ = this.pkgId$.pipe(
    switchMap(pkgId => this.patch.watch$('packageData', pkgId, 'status')),
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
      } else if (depError.type === 'actionRequired') {
        errorText = 'Action Required (see above)'
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
      // return this.formDialog.open<PackageConfigData>(ConfigModal, {
      //   label: `${pkg.currentDependencies[depId].title} config`,
      //   data: {
      //     pkgId: depId,
      //     dependentInfo: pkgManifest,
      //   },
      // })
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

    await this.router.navigate(['portal', 'marketplace'], navigationExtras)
  }
}

function toHealthCheck(
  status: T.MainStatus,
): T.NamedHealthCheckResult[] | null {
  return status.main !== 'running' || isEmptyObject(status.health)
    ? null
    : Object.values(status.health)
}
