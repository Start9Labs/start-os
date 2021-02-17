import { PollSource, RxStore, Source, WebsocketSource } from "patch-db-client"
import { ConfigService } from "src/app/services/config.service"
import { DataModel } from "./data-model"
import { LocalStorageBootstrap } from "./local-storage-bootstrap"
import { PatchDbModel } from "./patch-db-model"
import { ApiService } from "src/app/services/patch-api/api.service"

export function PatchDbModelFactory (
  config: ConfigService,
  bootstrap: LocalStorageBootstrap,
  api: ApiService,
): PatchDbModel {
  const patch = config.patchDb

  let source: Source<DataModel>
  switch(patch.type) {
    case 'poll': source = new PollSource({ ...patch }, api); break;
    case 'ws': source = new WebsocketSource({ ...patch }); break
  }

  const store = new RxStore<DataModel>({} as any)
  return new PatchDbModel({ store, http: api, sources: [source, api], bootstrap })
}