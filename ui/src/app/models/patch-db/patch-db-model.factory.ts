import { PollSource, RxStore, Source, WebsocketSource } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from './data-model'
import { LocalStorageBootstrap } from './local-storage-bootstrap'
import { PatchDbModel } from './patch-db-model'
import { ApiService } from 'src/app/services/patch-api/api.service'

export function PatchDbModelFactory (
  config: ConfigService,
  bootstrap: LocalStorageBootstrap,
  api: ApiService,
): PatchDbModel {
  const { patchDb : { usePollOverride, poll, websocket, timeoutForMissingPatch }, isConsulate } = config

  let source: Source<DataModel>
  if (isConsulate || usePollOverride) {
    source = new PollSource({ ...poll }, api)
  } else {
    source = new WebsocketSource({ ...websocket })
  }

  const store = new RxStore<DataModel>({ } as any)
  return new PatchDbModel({ store, http: api, sources: [source, api], bootstrap, timeoutForMissingPatch })
}