import { ChangeDetectionStrategy, Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import {
  AllPackageData,
  DataModel,
  InstallingState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { map, tap } from 'rxjs/operators'
import { ActivatedRoute, NavigationExtras } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { DependentInfo } from 'src/app/types/dependent-info'
import {
  DepErrorService,
  PkgDependencyErrors,
} from 'src/app/services/dep-error.service'
import { combineLatest } from 'rxjs'
import {
  getManifest,
  isInstalled,
  isInstalling,
  isRestoring,
  isUpdating,
} from 'src/app/util/get-package-data'
import { T } from '@start9labs/start-sdk'
import { ActionService } from 'src/app/services/action.service'

export interface DependencyInfo {
  id: string
  title: string
  icon: string
  version: string
  errorText: string
  actionText: string
  action: () => any
}

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowPage {
  private readonly pkgId = getPkgId(this.route)

  readonly pkgPlus$ = combineLatest([
    this.patch.watch$('packageData'),
    this.depErrorService.getPkgDepErrors$(this.pkgId),
  ]).pipe(
    tap(([allPkgs, _]) => {
      const pkg = allPkgs[this.pkgId]
      // if package disappears, navigate to list page
      if (!pkg) this.navCtrl.navigateRoot('/services')
    }),
    map(([allPkgs, depErrors]) => {
      const pkg = allPkgs[this.pkgId]
      return {
        pkg,
        dependencies: this.getDepInfo(pkg, allPkgs, depErrors),
        status: renderPkgStatus(pkg, depErrors),
      }
    }),
  )

  isInstalled = isInstalled

  constructor(
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    private readonly depErrorService: DepErrorService,
    private readonly actionService: ActionService,
  ) {}

  showProgress(
    pkg: PackageDataEntry,
  ): pkg is PackageDataEntry<InstallingState | UpdatingState> {
    return isInstalling(pkg) || isUpdating(pkg) || isRestoring(pkg)
  }

  private getDepInfo(
    pkg: PackageDataEntry,
    allPkgs: AllPackageData,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo[] {
    const manifest = getManifest(pkg)

    return (
      Object.keys(pkg.currentDependencies)
        // @TODO Aiden still need to filter self from currentDependencies?
        .filter(id => !!manifest.dependencies[id])
        .map(id => this.getDepValues(pkg, allPkgs, manifest, id, depErrors))
    )
  }

  private getDepDetails(
    pkg: PackageDataEntry,
    allPkgs: AllPackageData,
    depId: string,
  ) {
    const { title, icon, versionRange } = pkg.currentDependencies[depId]

    if (
      allPkgs[depId] &&
      (allPkgs[depId].stateInfo.state === 'installed' ||
        allPkgs[depId].stateInfo.state === 'updating')
    ) {
      return {
        title: allPkgs[depId].stateInfo.manifest!.title,
        icon: allPkgs[depId].icon,
        versionRange,
      }
    } else {
      return {
        title: title ? title : depId,
        icon: icon ? icon : 'assets/img/service-icons/fallback.png',
        versionRange,
      }
    }
  }

  private getDepValues(
    pkg: PackageDataEntry,
    allPkgs: AllPackageData,
    manifest: T.Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo {
    const { errorText, fixText, fixAction } = this.getDepErrors(
      pkg,
      manifest,
      depId,
      depErrors,
    )

    const { title, icon, versionRange } = this.getDepDetails(
      pkg,
      allPkgs,
      depId,
    )

    return {
      id: depId,
      version: versionRange,
      title,
      icon,
      errorText: errorText
        ? `${errorText}. ${manifest.title} will not work as expected.`
        : '',
      actionText: fixText || 'View',
      action:
        fixAction || (() => this.navCtrl.navigateForward(`/services/${depId}`)),
    }
  }

  private getDepErrors(
    pkg: PackageDataEntry,
    manifest: T.Manifest,
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
        fixAction = () => this.fixDep(pkg, manifest, 'install', depId)
      } else if (depError.type === 'incorrectVersion') {
        errorText = 'Incorrect version'
        fixText = 'Update'
        fixAction = () => this.fixDep(pkg, manifest, 'update', depId)
      } else if (depError.type === 'configUnsatisfied') {
        errorText = 'Config not satisfied'
        fixText = 'Auto config'
        fixAction = () => this.fixDep(pkg, manifest, 'configure', depId)
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

  private async fixDep(
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
        return this.actionService.present(
          { id: depId, title: '', mainStatus: 'running' },
          { id: 'config', metadata: pkg.actions['config'] },
          {
            title: pkgManifest.title,
            request: Object.values(pkg.requestedActions).find(
              r =>
                r.active &&
                r.request.packageId === depId &&
                r.request.actionId === 'config',
            )!.request,
          },
        )
    }
  }

  private async installDep(
    pkg: PackageDataEntry,
    pkgManifest: T.Manifest,
    depId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: pkgManifest.id,
      title: pkgManifest.title,
      version: pkg.currentDependencies[depId].versionRange,
    }
    const navigationExtras: NavigationExtras = {
      state: { dependentInfo },
    }

    await this.navCtrl.navigateForward(
      `/marketplace/${depId}`,
      navigationExtras,
    )
  }
}
