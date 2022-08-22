import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { filter, firstValueFrom } from 'rxjs'

export function getPackage(
  patch: PatchDbService,
  id: string,
): Promise<PackageDataEntry> {
  return firstValueFrom(patch.watch$('package-data', id))
}

export function getAllPackages(
  patch: PatchDbService,
): Promise<Record<string, PackageDataEntry>> {
  return firstValueFrom(patch.watch$('package-data').pipe(filter(Boolean)))
}
