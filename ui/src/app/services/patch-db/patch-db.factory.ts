import { PollSource, Source, WebsocketSource } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from './data-model'
import { LocalStorageBootstrap } from './local-storage-bootstrap'
import { PatchDbService } from './patch-db.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'

export function PatchDbServiceFactory (
  config: ConfigService,
  bootstrapper: LocalStorageBootstrap,
  embassyApi: ApiService,
): PatchDbService {

  const { mocks, patchDb: { poll }, supportsWebSockets } = config

  let source: Source<DataModel>

  if (mocks.enabled) {
    if (mocks.connection === 'poll') {
      source = new PollSource({ ...poll }, embassyApi)
    } else {
      source = new WebsocketSource(`ws://localhost:${config.mocks.wsPort}/db`)
    }
  } else {
    if (!supportsWebSockets) {
      source = new PollSource({ ...poll }, embassyApi)
    } else {
      const protocol = window.location.protocol === 'http:' ? 'ws' : 'wss'
      const host = window.location.host
      source = new WebsocketSource(`${protocol}://${host}/ws/db`)
    }
  }

  return new PatchDbService(source, embassyApi, bootstrapper)
}