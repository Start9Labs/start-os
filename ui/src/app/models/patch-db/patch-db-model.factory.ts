import { PollSource, Source, WebsocketSource } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from './data-model'
import { LocalStorageBootstrap } from './local-storage-bootstrap'
import { PatchDbModel } from './patch-db-model'
import { ApiService } from 'src/app/services/api/api.service'

export function PatchDbModelFactory (
  config: ConfigService,
  bootstrapper: LocalStorageBootstrap,
  http: ApiService,
): PatchDbModel {

  const { mocks, patchDb: { poll, timeoutForMissingRevision }, isConsulate } = config

  let source: Source<DataModel>

  if (mocks.enabled) {
    if (mocks.connection === 'poll') {
      source = new PollSource({ ...poll }, http)
    } else {
      source = new WebsocketSource(`ws://localhost:${config.mocks.wsPort}/db`)
    }
  } else {
    if (isConsulate) {
      source = new PollSource({ ...poll }, http)
    } else {
      const protocol = window.location.protocol === 'http:' ? 'ws' : 'wss'
      const host = window.location.host
      source = new WebsocketSource(`${protocol}://${host}/ws/db`)
    }
  }

  return new PatchDbModel({ sources: [source, http], bootstrapper, http, timeoutForMissingRevision })
}