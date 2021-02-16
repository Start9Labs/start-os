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

  const { patchDb: { usePollOverride, poll, websocket, timeoutForMissingRevision }, isConsulate } = config

  let source: Source<DataModel>
  if (isConsulate || usePollOverride) {
    source = new PollSource({ ...poll }, http)
  } else {
    source = new WebsocketSource({ ...websocket })
  }

  return new PatchDbModel({ sources: [source, http], bootstrapper, http, timeoutForMissingRevision })
}