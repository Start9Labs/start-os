import { Pipe, PipeTransform } from '@angular/core'
import { Manifest, PackageDataEntry } from '../../services/patch-db/data-model'
import { hasUi } from '../../services/config.service'
import { getManifest } from 'src/app/util/get-package-data'

@Pipe({
  name: 'hasUi',
})
export class UiPipe implements PipeTransform {
  transform(interfaces: PackageDataEntry['service-interfaces']): boolean {
    return interfaces ? hasUi(interfaces) : false
  }
}

@Pipe({
  name: 'toManifest',
})
export class ToManifestPipe implements PipeTransform {
  transform(pkg: PackageDataEntry): Manifest {
    return getManifest(pkg)
  }
}
