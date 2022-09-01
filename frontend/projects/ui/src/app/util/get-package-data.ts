import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { filter, firstValueFrom } from 'rxjs'

export function getPackage(
  patch: PatchDB<DataModel>,
  id: string,
): Promise<PackageDataEntry> {
  return firstValueFrom(patch.watch$('package-data', id))
}

export function getAllPackages(
  patch: PatchDB<DataModel>,
): Promise<Record<string, PackageDataEntry>> {
  return firstValueFrom(patch.watch$('package-data').pipe(filter(Boolean)))
}
