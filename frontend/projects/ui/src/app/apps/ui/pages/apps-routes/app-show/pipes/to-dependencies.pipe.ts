import { Pipe, PipeTransform } from '@angular/core'
import { NavigationExtras } from '@angular/router'
import { NavController } from '@ionic/angular'
import {
  DependencyErrorType,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { DependentInfo } from 'src/app/types/dependent-info'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  AppConfigPage,
  PackageConfigData,
} from 'src/app/modals/app-config/app-config.page'
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

@Pipe({
  name: 'toDependencies',
})
export class ToDependenciesPipe implements PipeTransform {
  constructor(
    private readonly navCtrl: NavController,
    private readonly formDialog: FormDialogService,
  ) {}

  transform(pkg: PackageDataEntry): DependencyInfo[] {
    if (!pkg.installed) return []

    return Object.keys(pkg.installed['current-dependencies'])
      .filter(depId => !!pkg.manifest.dependencies[depId])
      .map(depId => this.setDepValues(pkg, depId))
  }

  private setDepValues(pkg: PackageDataEntry, depId: string): DependencyInfo {
    let errorText = ''
    let actionText = 'View'
    let action: () => any = () =>
      this.navCtrl.navigateForward(`/services/${depId}`)

    const error = pkg.installed!.status['dependency-errors'][depId]

    if (error) {
      // health checks failed
      if (error.type === DependencyErrorType.HealthChecksFailed) {
        errorText = 'Health check failed'
        // not installed
      } else if (error.type === DependencyErrorType.NotInstalled) {
        errorText = 'Not installed'
        actionText = 'Install'
        action = () => this.fixDep(pkg, 'install', depId)
        // incorrect version
      } else if (error.type === DependencyErrorType.IncorrectVersion) {
        errorText = 'Incorrect version'
        actionText = 'Update'
        action = () => this.fixDep(pkg, 'update', depId)
        // not running
      } else if (error.type === DependencyErrorType.NotRunning) {
        errorText = 'Not running'
        actionText = 'Start'
        // config unsatisfied
      } else if (error.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config not satisfied'
        actionText = 'Auto config'
        action = () => this.fixDep(pkg, 'configure', depId)
      } else if (error.type === DependencyErrorType.Transitive) {
        errorText = 'Dependency has a dependency issue'
      }
      errorText = `${errorText}. ${pkg.manifest.title} will not work as expected.`
    }

    const depInfo = pkg.installed!['dependency-info'][depId]

    return {
      id: depId,
      version: pkg.manifest.dependencies[depId].version,
      title: depInfo?.title || depId,
      icon: depInfo?.icon || '',
      errorText,
      actionText,
      action,
    }
  }

  async fixDep(
    pkg: PackageDataEntry,
    action: 'install' | 'update' | 'configure',
    depId: string,
  ): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(pkg.manifest, depId)
      case 'configure':
        return this.formDialog.open<PackageConfigData>(AppConfigPage, {
          label: `${
            pkg.installed!['dependency-info'][depId].title
          } configuration`,
          data: {
            pkgId: depId,
            dependentInfo: pkg.manifest,
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
