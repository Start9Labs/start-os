import { Http, LiveHttp as LivePatchDbHttp, PollSource, RxStore, Source, WebsocketSource } from "patch-db-client"
import { ConfigService } from "src/app/services/config.service"
import { DataModel } from "./data-model"
import { LocalStorageBootstrap } from "./local-storage-bootstrap"
import { PatchDbModel } from "./patch-db-model"
import { MockPatchDbHttp } from "./mock-http"
import { ImperativeSource } from "./imperative-source"

export function PatchDbModelFactory (
  config: ConfigService,
  bootstrap: LocalStorageBootstrap,
  imperativeSource: ImperativeSource,
): PatchDbModel {
  const { http : httpC, source : sourceC } = config.patchDb

  let http: Http<DataModel>
  switch(httpC.type) {
    case 'mock': http = new MockPatchDbHttp(); break;
    case 'live': http = new LivePatchDbHttp(httpC.url)
  }

  let source: Source<DataModel>
  switch(sourceC.type) {
    case 'poll': source = new PollSource({ ...sourceC }, http); break;
    case 'ws': source = new WebsocketSource({ ...sourceC }); break
  }

  const store = new RxStore<DataModel>({} as any)
  return new PatchDbModel({ store, http, sources: [source, imperativeSource], bootstrap })
}