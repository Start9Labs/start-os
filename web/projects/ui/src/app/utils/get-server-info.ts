import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'

export async function getServerInfo(
  patch: PatchDB<DataModel>,
): Promise<DataModel['serverInfo']> {
  return firstValueFrom(patch.watch$('serverInfo'))
}
