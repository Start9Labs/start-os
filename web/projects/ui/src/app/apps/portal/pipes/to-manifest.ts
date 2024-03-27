import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Manifest } from '@start9labs/marketplace'
import { getManifest } from 'src/app/util/get-package-data'

@Pipe({
  name: 'toManifest',
  standalone: true,
})
export class ToManifestPipe implements PipeTransform {
  transform(pkg: PackageDataEntry): Manifest {
    return getManifest(pkg)
  }
}