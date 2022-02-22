import { Pipe, PipeTransform } from '@angular/core'
import { NavigationExtras } from '@angular/router'
import { NavController } from '@ionic/angular'
import { combineLatest, Observable } from 'rxjs'
import { filter, map, startWith } from 'rxjs/operators'
import {
  DependencyError,
  DependencyErrorType,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { DependentInfo, exists } from '@start9labs/shared'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ModalService } from 'src/app/services/modal.service'

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
    private readonly patch: PatchDbService,
    private readonly navCtrl: NavController,
    private readonly modalService: ModalService,
  ) {}

  transform(pkg: PackageDataEntry): Observable<DependencyInfo[]> {
    return combineLatest([
      this.patch.watch$(
        'package-data',
        pkg.manifest.id,
        'installed',
        'current-dependencies',
      ),
      this.patch.watch$(
        'package-data',
        pkg.manifest.id,
        'installed',
        'status',
        'dependency-errors',
      ),
    ]).pipe(
      filter(deps => deps.every(exists) && !!pkg.installed),
      map(([currentDeps, depErrors]) =>
        Object.keys(currentDeps)
          .filter(id => !!pkg.manifest.dependencies[id])
          .map(id => this.setDepValues(pkg, id, depErrors)),
      ),
      startWith([]),
    )
  }

  private setDepValues(
    pkg: PackageDataEntry,
    id: string,
    errors: { [id: string]: DependencyError },
  ): DependencyInfo {
    let errorText = ''
    let actionText = 'View'
    let action: () => any = () =>
      this.navCtrl.navigateForward(`/services/${id}`)

    const error = errors[id]

    if (error) {
      // health checks failed
      if (
        [
          DependencyErrorType.InterfaceHealthChecksFailed,
          DependencyErrorType.HealthChecksFailed,
        ].includes(error.type)
      ) {
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

    const depInfo = pkg.installed['dependency-info'][id]

    return {
      id,
      version: pkg.manifest.dependencies[id].version,
      title: depInfo.manifest?.title || id,
      icon: depInfo.icon,
      errorText,
      actionText,
      action,
    }
  }

  async fixDep(
    pkg: PackageDataEntry,
    action: 'install' | 'update' | 'configure',
    id: string,
  ): Promise<void> {
    switch (action) {
      case 'install':
      case 'update':
        return this.installDep(pkg, id)
      case 'configure':
        return this.configureDep(pkg, id)
    }
  }

  private async installDep(
    pkg: PackageDataEntry,
    depId: string,
  ): Promise<void> {
    const version = pkg.manifest.dependencies[depId].version

    const dependentInfo: DependentInfo = {
      id: pkg.manifest.id,
      title: pkg.manifest.title,
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
    pkg: PackageDataEntry,
    dependencyId: string,
  ): Promise<void> {
    const dependentInfo: DependentInfo = {
      id: pkg.manifest.id,
      title: pkg.manifest.title,
    }

    await this.modalService.presentModalConfig({
      pkgId: dependencyId,
      dependentInfo,
    })
  }
}
