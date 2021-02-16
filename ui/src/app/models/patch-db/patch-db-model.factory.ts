import { Http, LiveHttp as LivePatchDbHttp, PollSource, RxStore, Source, WebsocketSource } from "patch-db-client"
import { ConfigService } from "src/app/services/config.service"
import { DataModel } from "./data-model"
import { LocalStorageBootstrap } from "./local-storage-bootstrap"
import { PatchDbModel } from "./patch-db-model"

export function PatchDbModelFactory (
  config: ConfigService['patchDb'],
  bootstrap: LocalStorageBootstrap,
): PatchDbModel {
  let http: Http<DataModel>
  switch(config.http.type) {
    case 'mock': http = new MockPatchDbHttp(); break;
    case 'live': http = new LivePatchDbHttp(config.http.url)
  }
  let source: Source<DataModel>
  switch(config.source.type) {
    case 'poll': source = new PollSource({ ...config.source }, http); break;
    case 'ws': source = new WebsocketSource({ ...config.source }); break
  }

  const store = new RxStore<DataModel>({} as any)
  return new PatchDbModel({ store, http, source, bootstrap })
}