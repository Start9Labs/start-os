import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Pipe({
  name: 'toManifest',
})
export class ToManifestPipe implements PipeTransform {
  transform(pkg: PackageDataEntry) {
    return getManifest(pkg)
  }
}
