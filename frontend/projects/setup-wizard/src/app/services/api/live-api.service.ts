import { Injectable } from '@angular/core'
import {
  DiskListResponse,
  EmbassyOSDiskInfo,
  encodeBase64,
  HttpService,
  isRpcError,
  Log,
  RpcError,
  RPCOptions,
} from '@start9labs/shared'
import {
  ApiService,
  CifsRecoverySource,
  DiskRecoverySource,
  StatusRes,
  AttachReq,
  ExecuteReq,
  CompleteRes,
} from './api.service'
import * as jose from 'node-jose'
import { webSocket } from 'rxjs/webSocket'
import { Observable } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  constructor(private readonly http: HttpService) {
    super()
  }

  async getStatus() {
    return this.rpcRequest<StatusRes>({
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
  async getPubKey() {
    const response: jose.JWK.Key = await this.rpcRequest({
      method: 'setup.get-pubkey',
      params: {},
    })

    this.pubkey = response
  }

  async getDrives() {
    return this.rpcRequest<DiskListResponse>({
      method: 'setup.disk.list',
      params: {},
    })
  }

  async verifyCifs(source: CifsRecoverySource) {
    source.path = source.path.replace('/\\/g', '/')
    return this.rpcRequest<EmbassyOSDiskInfo>({
      method: 'setup.cifs.verify',
      params: source,
    })
  }

  async attach(params: AttachReq) {
    await this.rpcRequest<void>({
      method: 'setup.attach',
      params,
    })
  }

  async execute(setupInfo: ExecuteReq) {
    if (setupInfo['recovery-source']?.type === 'backup') {
      if (isCifsSource(setupInfo['recovery-source'].target)) {
        setupInfo['recovery-source'].target.path = setupInfo[
          'recovery-source'
        ].target.path.replace('/\\/g', '/')
      }
    }

    await this.rpcRequest<void>({
      method: 'setup.execute',
      params: setupInfo,
    })
  }

  async followLogs(): Promise<string> {
    return this.rpcRequest({ method: 'setup.logs.follow', params: {} })
  }

  openLogsWebsocket$(guid: string): Observable<Log> {
    return webSocket(`http://start.local/ws/${guid}`)
  }

  async complete() {
    const res = await this.rpcRequest<CompleteRes>({
      method: 'setup.complete',
      params: {},
    })

    return {
      ...res,
      'root-ca': encodeBase64(res['root-ca']),
    }
  }

  async exit() {
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
  source: CifsRecoverySource | DiskRecoverySource | null,
): source is CifsRecoverySource {
  return !!(source as CifsRecoverySource)?.hostname
}
