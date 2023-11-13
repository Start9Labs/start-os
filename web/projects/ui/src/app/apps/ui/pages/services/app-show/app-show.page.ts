import { ChangeDetectionStrategy, Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
  PackageState,
  InstalledPackageInfo,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryStatus,
  renderPkgStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { map, tap } from 'rxjs/operators'
import { ActivatedRoute, NavigationExtras } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { DependentInfo } from 'src/app/types/dependent-info'
import {
  DepErrorService,
  DependencyErrorType,
  PkgDependencyErrors,
} from 'src/app/services/dep-error.service'
import { combineLatest } from 'rxjs'
import { Manifest } from '@start9labs/marketplace'
import {
  AppConfigPage,
  PackageConfigData,
} from './modals/app-config/app-config.page'
import { FormDialogService } from 'src/app/services/form-dialog.service'

export interface DependencyInfo {
  id: string
  title: string
  icon: string
  version: string
  errorText: string
  actionText: string
  action: () => any
}

const STATES = [
  PackageState.Installing,
  PackageState.Updating,
  PackageState.Restoring,
]

@Component({
  selector: 'app-show',
  templateUrl: './app-show.page.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowPage {
  readonly pkgId = getPkgId(this.route)

  readonly pkgPlus$ = combineLatest([
    this.patch.watch$('package-data', this.pkgId),
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

  constructor(
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    private readonly depErrorService: DepErrorService,
    private readonly formDialog: FormDialogService,
  ) {}

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
    pkg: PackageDataEntry,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo[] {
    const pkgInstalled = pkg.installed

    if (!pkgInstalled) return []

    const pkgManifest = pkg.manifest

    return Object.keys(pkgInstalled['current-dependencies'])
      .filter(depId => !!pkg.manifest.dependencies[depId])
      .map(depId =>
        this.getDepValues(pkgInstalled, pkgManifest, depId, depErrors),
      )
  }

  private getDepValues(
    pkgInstalled: InstalledPackageInfo,
    pkgManifest: Manifest,
    depId: string,
    depErrors: PkgDependencyErrors,
  ): DependencyInfo {
    const { errorText, fixText, fixAction } = this.getDepErrors(
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
        fixAction || (() => this.navCtrl.navigateForward(`/services/${depId}`)),
    }
  }

  private getDepErrors(
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
        fixAction = () => this.fixDep(pkgManifest, 'install', depId)
      } else if (depError.type === DependencyErrorType.IncorrectVersion) {
        errorText = 'Incorrect version'
        fixText = 'Update'
        fixAction = () => this.fixDep(pkgManifest, 'update', depId)
      } else if (depError.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config not satisfied'
        fixText = 'Auto config'
        fixAction = () => this.fixDep(pkgManifest, 'configure', depId)
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
    pkgManifest: Manifest,
    action: 'install' | 'update' | 'configure',
    id: string,
  ): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(pkgManifest, id)
      case 'configure':
        return this.configureDep(pkgManifest, id)
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

    await this.navCtrl.navigateForward(
      `/marketplace/${depId}`,
      navigationExtras,
    )
  }

  private async configureDep(
    manifest: Manifest,
    dependencyId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: manifest.id,
      title: manifest.title,
    }

    return this.formDialog.open<PackageConfigData>(AppConfigPage, {
      label: 'Config',
      data: {
        pkgId: dependencyId,
        dependentInfo,
      },
    })
  }
}
