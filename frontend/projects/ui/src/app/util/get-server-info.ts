import { PatchDB } from 'patch-db-client'
import { DataModel, ServerInfo } from 'src/app/services/patch-db/data-model'
import { filter, firstValueFrom } from 'rxjs'

export function getServerInfo(patch: PatchDB<DataModel>): Promise<ServerInfo> {
  return firstValueFrom(patch.watch$('server-info').pipe(filter(Boolean)))
}
