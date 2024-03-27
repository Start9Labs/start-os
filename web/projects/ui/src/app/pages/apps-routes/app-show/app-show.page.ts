import { ChangeDetectionStrategy, Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  InstallingState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { renderPkgStatus } from 'src/app/services/pkg-status-rendering.service'
import { map, tap } from 'rxjs/operators'
import { ActivatedRoute, NavigationExtras } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { ModalService } from 'src/app/services/modal.service'
import { DependentInfo } from 'src/app/types/dependent-info'
import {
  DepErrorService,
  DependencyErrorType,
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
import { Manifest } from '@start9labs/marketplace'

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
    this.patch.watch$('packageData', this.pkgId),
    this.depErrorService.getPkgDepErrors$(this.pkgId),
  ]).pipe(
    tap(([pkg, _]) => {
      // if package disappears, navigate to list page
      if (!pkg) this.navCtrl.navigateRoot('/services')
    }),
    map(([pkg, depErrors]) => {
      return {
        pkg,
        dependencies: this.getDepInfo(pkg, depErrors),
        status: renderPkgStatus(pkg, depErrors),
      }
    }),
  )

  isInstalled = isInstalled

  constructor(
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    private readonly modalService: ModalService,
    private readonly depErrorService: DepErrorService,
  ) {}

  showProgress(
    pkg: PackageDataEntry,
  ): pkg is PackageDataEntry<InstallingState | UpdatingState> {
    return isInstalling(pkg) || isUpdating(pkg) || isRestoring(pkg)
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
    manifest: Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo {
    const { errorText, fixText, fixAction } = this.getDepErrors(
      pkg,
      manifest,
      depId,
      depErrors,
    )

    const { title, icon, versionSpec } = pkg.currentDependencies[depId]

    return {
      id: depId,
      version: versionSpec,
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
    manifest: Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ) {
    const depError = depErrors[depId]

    let errorText: string | null = null
    let fixText: string | null = null
    let fixAction: (() => any) | null = null

    if (depError) {
      if (depError.type === DependencyErrorType.NotInstalled) {
        errorText = 'Not installed'
        fixText = 'Install'
        fixAction = () => this.fixDep(pkg, manifest, 'install', depId)
      } else if (depError.type === DependencyErrorType.IncorrectVersion) {
        errorText = 'Incorrect version'
        fixText = 'Update'
        fixAction = () => this.fixDep(pkg, manifest, 'update', depId)
      } else if (depError.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config not satisfied'
        fixText = 'Auto config'
        fixAction = () => this.fixDep(pkg, manifest, 'configure', depId)
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

  private async fixDep(
    pkg: PackageDataEntry,
    pkgManifest: Manifest,
    action: 'install' | 'update' | 'configure',
    id: string,
  ): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(pkg, pkgManifest, id)
      case 'configure':
        return this.configureDep(pkgManifest, id)
    }
  }

  private async installDep(
    pkg: PackageDataEntry,
    pkgManifest: Manifest,
    depId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: pkgManifest.id,
      title: pkgManifest.title,
      version: pkg.currentDependencies[depId].versionSpec,
    }
    const navigationExtras: NavigationExtras = {
      state: { dependentInfo },
    }

    await this.navCtrl.navigateForward(
      `/marketplace/${depId}`,
      navigationExtras,
    )
  }

  private async configureDep(
    pkgManifest: Manifest,
    dependencyId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: pkgManifest.id,
      title: pkgManifest.title,
    }

    await this.modalService.presentModalConfig({
      pkgId: dependencyId,
      dependentInfo,
    })
  }
}
