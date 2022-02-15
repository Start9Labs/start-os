import { MockSource, PollSource, WebsocketSource } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { LocalStorageBootstrap } from './local-storage-bootstrap'
import { PatchDbService } from './patch-db.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from '../auth.service'
import { MockApiService } from '../api/embassy-mock-api.service'
import { filter } from 'rxjs/operators'
import { exists } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Storage } from '@ionic/storage-angular'

export function PatchDbServiceFactory(
  config: ConfigService,
  embassyApi: ApiService,
  bootstrapper: LocalStorageBootstrap,
  auth: AuthService,
  storage: Storage,
): PatchDbService {
  const {
    useMocks,
    patchDb: { poll },
  } = config

  if (useMocks) {
    const source = new MockSource<DataModel>(
      (embassyApi as MockApiService).mockPatch$.pipe(filter(exists)),
    )
    return new PatchDbService(
      source,
      source,
      embassyApi,
      bootstrapper,
      auth,
      storage,
    )
  } else {
    const protocol = window.location.protocol === 'http:' ? 'ws' : 'wss'
    const host = window.location.host
    const wsSource = new WebsocketSource<DataModel>(
      `${protocol}://${host}/ws/db`,
    )
    const pollSource = new PollSource<DataModel>({ ...poll }, embassyApi)

    return new PatchDbService(
      wsSource,
      pollSource,
      embassyApi,
      bootstrapper,
      auth,
      storage,
    )
  }
}
