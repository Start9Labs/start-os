import { PatchDB } from 'patch-db-client'
import { DataModel, ServerInfo } from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'

export async function getServerInfo(
  patch: PatchDB<DataModel>,
): Promise<ServerInfo> {
  return firstValueFrom(patch.watch$('server-info'))
}
