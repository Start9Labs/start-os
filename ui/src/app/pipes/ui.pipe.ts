import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from '../services/patch-db/data-model'
import { ConfigService, hasUi } from '../services/config.service'

@Pipe({
  name: 'hasUi',
})
export class HasUiPipe implements PipeTransform {

  transform (pkg: PackageDataEntry): boolean {
    const interfaces = pkg.manifest.interfaces
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
