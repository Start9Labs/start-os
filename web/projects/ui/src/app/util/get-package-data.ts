import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'

export async function getPackage(
  patch: PatchDB<DataModel>,
  id: string,
): Promise<PackageDataEntry | undefined> {
  return firstValueFrom(patch.watch$('package-data', id))
}

export async function getAllPackages(
  patch: PatchDB<DataModel>,
): Promise<DataModel['package-data']> {
  return firstValueFrom(patch.watch$('package-data'))
}
