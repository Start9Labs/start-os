import { Pipe, PipeTransform } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { filter, map, startWith, Observable } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getPackageInfo } from 'src/app/util/get-package-info'
import { PkgInfo } from 'src/app/types/pkg-info'

@Pipe({
  name: 'packageInfo',
})
export class PackageInfoPipe implements PipeTransform {
  constructor(private readonly patch: PatchDB<DataModel>) {}

  transform(pkg: PackageDataEntry): Observable<PkgInfo> {
    return this.patch
      .watch$('package-data', pkg.manifest.id)
      .pipe(filter(Boolean), startWith(pkg), map(getPackageInfo))
  }
}
