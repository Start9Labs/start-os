import { Pipe, PipeTransform } from '@angular/core'
import { Observable } from 'rxjs'
import { filter, map, startWith } from 'rxjs/operators'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getPackageInfo, PkgInfo } from '../../../util/get-package-info'
import { PatchDB } from 'patch-db-client'

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
