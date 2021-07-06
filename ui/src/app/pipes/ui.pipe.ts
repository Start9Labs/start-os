import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry, Manifest } from '../services/patch-db/data-model'
import { ConfigService, getManifest, hasUi } from '../services/config.service'

@Pipe({
  name: 'hasUi',
})
export class HasUiPipe implements PipeTransform {

  transform (pkg: PackageDataEntry): boolean {
    const interfaces = getManifest(pkg).interfaces
    return hasUi(interfaces)
  }
}

@Pipe({
  name: 'isLaunchable',
})
export class LaunchablePipe implements PipeTransform {

  constructor (private configService: ConfigService) { }

  transform (pkg: PackageDataEntry): boolean {
    return this.configService.isLaunchable(pkg)
  }
}

@Pipe({
  name: 'manifest',
})
export class ManifestPipe implements PipeTransform {

  transform (pkg: PackageDataEntry): Manifest {
    return getManifest(pkg)
  }
}
