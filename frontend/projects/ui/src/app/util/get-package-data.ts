import { first } from 'rxjs/operators'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

export function getPackage(
  patch: PatchDbService,
  id: string,
): Promise<PackageDataEntry> {
  return patch.watch$('package-data', id).pipe(first()).toPromise()
}

export function getAllPackages(
  patch: PatchDbService,
): Promise<Record<string, PackageDataEntry>> {
  return patch.watch$('package-data').pipe(first()).toPromise()
}
