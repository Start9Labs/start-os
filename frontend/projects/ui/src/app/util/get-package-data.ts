import { first } from 'rxjs/operators'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

export function getPackageData(
  patch: PatchDbService,
): Promise<Record<string, PackageDataEntry>> {
  return patch.watch$('package-data').pipe(first()).toPromise()
}
