import { first } from 'rxjs/operators'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ServerInfo } from 'src/app/services/patch-db/data-model'

export function getServerInfo(patch: PatchDbService): Promise<ServerInfo> {
  return patch.watch$('server-info').pipe(first()).toPromise()
}
