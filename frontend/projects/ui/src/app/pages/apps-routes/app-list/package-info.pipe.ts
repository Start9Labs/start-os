import { Pipe, PipeTransform } from '@angular/core'
import { Observable, combineLatest, firstValueFrom } from 'rxjs'
import { map } from 'rxjs/operators'
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
    firstValueFrom(this.patch.watch$('package-data', pkgId))

    return combineLatest([
      this.patch.watch$('package-data', pkgId),
      this.depErrorService.getPkgDepErrors$(pkgId),
    ]).pipe(map(([pkg, depErrors]) => getPackageInfo(pkg, depErrors)))
  }
}
