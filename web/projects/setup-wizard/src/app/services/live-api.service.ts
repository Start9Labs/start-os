import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'
import {
  DiskListResponse,
  encodeBase64,
  FollowLogsReq,
  FollowLogsRes,
  HttpService,
  isRpcError,
  Log,
  RpcError,
  RPCOptions,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import * as jose from 'node-jose'
import { Observable } from 'rxjs'
import { webSocket, WebSocketSubjectConfig } from 'rxjs/webSocket'
import { ApiService } from './api.service'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  constructor(
    private readonly http: HttpService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {
    super()
  }

  openProgressWebsocket$(guid: string): Observable<T.FullProgress> {
    const { location } = this.document.defaultView!
    const host = location.host

    return webSocket({
      url: `ws://${host}/ws/rpc/${guid}`,
    })
  }

  async getStatus(): Promise<T.SetupStatusRes | null> {
    return this.rpcRequest<T.SetupStatusRes | null>({
      method: 'setup.status',
      params: {},
    })
  }

  /**
   * We want to update the pubkey, which means that we will call in clearnet the
   * getPubKey, and all the information is never in the clear, and only public
   * information is sent across the network. We don't want to expose that we do
   * this wil all public/private key, which means that there is no information loss
   * through the network.
   */
  async getPubKey(): Promise<void> {
    const response: jose.JWK.Key = await this.rpcRequest({
      method: 'setup.get-pubkey',
      params: {},
    })

    this.pubkey = response
  }

  async getDrives(): Promise<DiskListResponse> {
    return this.rpcRequest<DiskListResponse>({
      method: 'setup.disk.list',
      params: {},
    })
  }

  async verifyCifs(
    source: T.VerifyCifsParams,
  ): Promise<Record<string, StartOSDiskInfo>> {
    source.path = source.path.replace('/\\/g', '/')
    return this.rpcRequest<Record<string, StartOSDiskInfo>>({
      method: 'setup.cifs.verify',
      params: source,
    })
  }

  async attach(params: T.AttachParams): Promise<T.SetupProgress> {
    return this.rpcRequest<T.SetupProgress>({
      method: 'setup.attach',
      params,
    })
  }

  async execute(setupInfo: T.SetupExecuteParams): Promise<T.SetupProgress> {
    if (setupInfo.recoverySource?.type === 'backup') {
      if (isCifsSource(setupInfo.recoverySource.target)) {
        setupInfo.recoverySource.target.path =
          setupInfo.recoverySource.target.path.replace('/\\/g', '/')
      }
    }

    return this.rpcRequest<T.SetupProgress>({
      method: 'setup.execute',
      params: setupInfo,
    })
  }

  async followServerLogs(params: FollowLogsReq): Promise<FollowLogsRes> {
    return this.rpcRequest({ method: 'setup.logs.follow', params })
  }

  openLogsWebsocket$({ url }: WebSocketSubjectConfig<Log>): Observable<Log> {
    return webSocket(`http://start.local/ws/${url}`)
  }

  async complete(): Promise<T.SetupResult> {
    const res = await this.rpcRequest<T.SetupResult>({
      method: 'setup.complete',
      params: {},
    })

    return {
      ...res,
      rootCa: encodeBase64(res.rootCa),
    }
  }

  async exit(): Promise<void> {
    await this.rpcRequest<void>({
      method: 'setup.exit',
      params: {},
    })
  }

  private async rpcRequest<T>(opts: RPCOptions): Promise<T> {
    const res = await this.http.rpcRequest<T>(opts)

    const rpcRes = res.body

    if (isRpcError(rpcRes)) {
      throw new RpcError(rpcRes.error)
    }

    return rpcRes.result
  }
}

function isCifsSource(
  source: T.BackupTargetFS | null,
): source is T.Cifs & { type: 'cifs' } {
  return !!(source as T.Cifs)?.hostname
}
