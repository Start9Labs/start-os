import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { firstValueFrom } from 'rxjs'
import { T } from '@start9labs/start-sdk'

export async function getServerInfo(
  patch: PatchDB<DataModel>,
): Promise<T.ServerInfo> {
  return firstValueFrom(patch.watch$('serverInfo'))
}
