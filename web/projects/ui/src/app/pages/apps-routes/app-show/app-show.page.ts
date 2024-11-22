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
import { getDepDetails } from 'src/app/util/dep-info'

export interface DependencyInfo {
  id: string
  title: string
  icon: string
  version: string
  errorText: string
  actionText: string | null
  action: () => any
}

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowPage {
  readonly pkgId = getPkgId(this.route)

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
      const manifest = getManifest(pkg)
      return {
        allPkgs,
        pkg,
        manifest,
        dependencies: this.getDepInfo(pkg, manifest, allPkgs, depErrors),
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
  ) {}

  showProgress(
    pkg: PackageDataEntry,
  ): pkg is PackageDataEntry<InstallingState | UpdatingState> {
    return isInstalling(pkg) || isUpdating(pkg) || isRestoring(pkg)
  }

  private getDepInfo(
    pkg: PackageDataEntry,
    manifest: T.Manifest,
    allPkgs: AllPackageData,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo[] {
    return Object.keys(pkg.currentDependencies).map(id =>
      this.getDepValues(pkg, allPkgs, manifest, id, depErrors),
    )
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

    const { title, icon, versionRange } = getDepDetails(pkg, allPkgs, depId)

    return {
      id: depId,
      version: versionRange,
      title,
      icon,
      errorText: errorText ? errorText : '',
      actionText: fixText,
      action: fixAction,
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
    let fixAction: () => any = () => {}

    if (depError) {
      if (depError.type === 'notInstalled') {
        errorText = 'Not installed'
        fixText = 'Install'
        fixAction = () => this.installDep(pkg, manifest, depId)
      } else if (depError.type === 'incorrectVersion') {
        errorText = 'Incorrect version'
        fixText = 'Update'
        fixAction = () => this.installDep(pkg, manifest, depId)
      } else if (depError.type === 'actionRequired') {
        errorText = 'Action Required (see above)'
      } else if (depError.type === 'notRunning') {
        errorText = 'Not running'
        fixText = 'Start'
        fixAction = () => this.navCtrl.navigateForward(`/services/${depId}`)
      } else if (depError.type === 'healthChecksFailed') {
        errorText = 'Required health check not passing'
        fixText = 'View'
        fixAction = () => this.navCtrl.navigateForward(`/services/${depId}`)
      } else if (depError.type === 'transitive') {
        errorText = 'Dependency has a dependency issue'
        fixText = 'View'
        fixAction = () => this.navCtrl.navigateForward(`/services/${depId}`)
      }
    }

    return {
      errorText,
      fixText,
      fixAction,
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
