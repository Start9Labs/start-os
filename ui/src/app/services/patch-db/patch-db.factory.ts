import { MockSource, PollSource, Source, WebsocketSource } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from './data-model'
import { LocalStorageBootstrap } from './local-storage-bootstrap'
import { PatchDbService } from './patch-db.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from '../auth.service'
import { MockApiService } from '../api/embassy-mock-api.service'
import { filter } from 'rxjs/operators'
import { exists } from 'src/app/util/misc.util'

export function PatchDbServiceFactory (
  config: ConfigService,
  embassyApi: ApiService,
  bootstrapper: LocalStorageBootstrap,
  auth: AuthService,
): PatchDbService {

  const { mocks, patchDb: { poll }, supportsWebSockets } = config

  let source: Source<DataModel>

  if (mocks.enabled) {
    source = new MockSource((embassyApi as MockApiService).mockPatch$.pipe(filter(exists)))
  } else {
    if (!supportsWebSockets) {
      source = new PollSource({ ...poll }, embassyApi)
    } else {
      const protocol = window.location.protocol === 'http:' ? 'ws' : 'wss'
      const host = window.location.host
      source = new WebsocketSource(`${protocol}://${host}/ws/db`)
    }
  }

  return new PatchDbService(source, embassyApi, bootstrapper, auth)
}