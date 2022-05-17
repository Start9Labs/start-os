import { Pipe, PipeTransform } from '@angular/core'
import { Observable } from 'rxjs'
import { filter, map, startWith } from 'rxjs/operators'
import { PackageDataEntry } from '../../../services/patch-db/data-model'
import { getPackageInfo, PkgInfo } from '../../../util/get-package-info'
import { PatchDbService } from '../../../services/patch-db/patch-db.service'

@Pipe({
  name: 'packageInfo',
})
export class PackageInfoPipe implements PipeTransform {
  constructor(private readonly patch: PatchDbService) {}

  transform(pkg: PackageDataEntry): Observable<PkgInfo> {
    return this.patch
      .watch$('package-data', pkg.manifest.id)
      .pipe(
        filter<PackageDataEntry>(Boolean),
        startWith(pkg),
        map(getPackageInfo),
      )
  }
}
