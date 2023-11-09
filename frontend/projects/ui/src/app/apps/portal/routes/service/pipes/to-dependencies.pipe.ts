import { Pipe, PipeTransform } from '@angular/core'
import { NavigationExtras, Router } from '@angular/router'
import { Manifest } from '@start9labs/marketplace'
import {
  DependencyErrorType,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { DependentInfo } from 'src/app/types/dependent-info'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { ServiceConfigModal } from '../modals/config.component'
import { DependencyInfo } from '../types/dependency-info'
import { PackageConfigData } from '../types/package-config-data'
import { NavigationService } from '../../../services/navigation.service'
import { toRouterLink } from '../../../utils/to-router-link'

@Pipe({
  name: 'toDependencies',
  standalone: true,
})
export class ToDependenciesPipe implements PipeTransform {
  constructor(
    private readonly router: Router,
    private readonly formDialog: FormDialogService,
    private readonly navigation: NavigationService,
  ) {}

  transform(pkg: PackageDataEntry): DependencyInfo[] | null {
    if (!pkg.installed) return null

    const deps = Object.keys(pkg.installed['current-dependencies'])
      .filter(depId => pkg.manifest.dependencies[depId])
      .map(depId => this.setDepValues(pkg, depId))

    return deps.length ? deps : null
  }

  private setDepValues(pkg: PackageDataEntry, id: string): DependencyInfo {
    const error = pkg.installed!.status['dependency-errors'][id]
    const depInfo = pkg.installed!['dependency-info'][id]
    const version = pkg.manifest.dependencies[id].version
    const title = depInfo?.title || id
    const icon = depInfo?.icon || ''

    let errorText = ''
    let actionText = 'View'
    let action = () => {
      this.navigation.addTab({ icon, title, routerLink: toRouterLink(id) })
      this.router.navigate([`portal`, `service`, id])
    }

    if (error) {
      // health checks failed
      if (error.type === DependencyErrorType.HealthChecksFailed) {
        errorText = 'Health check failed'
        // not installed
      } else if (error.type === DependencyErrorType.NotInstalled) {
        errorText = 'Not installed'
        actionText = 'Install'
        action = () => this.fixDep(pkg, 'install', id)
        // incorrect version
      } else if (error.type === DependencyErrorType.IncorrectVersion) {
        errorText = 'Incorrect version'
        actionText = 'Update'
        action = () => this.fixDep(pkg, 'update', id)
        // not running
      } else if (error.type === DependencyErrorType.NotRunning) {
        errorText = 'Not running'
        actionText = 'Start'
        // config unsatisfied
      } else if (error.type === DependencyErrorType.ConfigUnsatisfied) {
        errorText = 'Config not satisfied'
        actionText = 'Auto config'
        action = () => this.fixDep(pkg, 'configure', id)
      } else if (error.type === DependencyErrorType.Transitive) {
        errorText = 'Dependency has a dependency issue'
      }
      errorText = `${errorText}. ${pkg.manifest.title} will not work as expected.`
    }

    return { id, icon, title, version, errorText, actionText, action }
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
        return this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
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

    await this.router.navigate(['marketplace', depId], navigationExtras)
  }

  private async configureDep(
    manifest: Manifest,
    dependencyId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: manifest.id,
      title: manifest.title,
    }

    return this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
      label: 'Config',
      data: {
        pkgId: dependencyId,
        dependentInfo,
      },
    })
  }
}
