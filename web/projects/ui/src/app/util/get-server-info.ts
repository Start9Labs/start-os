import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'
import { ServerInfo } from '../../../../../../core/startos/bindings/ServerInfo'

export async function getServerInfo(
  patch: PatchDB<DataModel>,
): Promise<ServerInfo> {
  return firstValueFrom(patch.watch$('serverInfo'))
}
