import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ServerInfo } from 'src/app/services/patch-db/data-model'
import { filter, firstValueFrom } from 'rxjs'

export function getServerInfo(patch: PatchDbService): Promise<ServerInfo> {
  return firstValueFrom(patch.watch$('server-info').pipe(filter(Boolean)))
}
