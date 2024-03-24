import { Pipe, PipeTransform } from '@angular/core'
import { Observable, combineLatest } from 'rxjs'
import { filter, map } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getPackageInfo, PkgInfo } from '../../../util/get-package-info'
import { PatchDB } from 'patch-db-client'
import { DepErrorService } from 'src/app/services/dep-error.service'

@Pipe({
  name: 'packageInfo',
})
export class PackageInfoPipe implements PipeTransform {
  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly depErrorService: DepErrorService,
  ) {}

  transform(pkgId: string): Observable<PkgInfo> {
    return combineLatest([
      this.patch.watch$('packageData', pkgId).pipe(filter(Boolean)),
      this.depErrorService.getPkgDepErrors$(pkgId),
    ]).pipe(map(([pkg, depErrors]) => getPackageInfo(pkg, depErrors)))
  }
}
