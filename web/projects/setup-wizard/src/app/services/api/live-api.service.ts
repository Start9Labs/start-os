import { Injectable } from '@angular/core'
import {
  DiskListResponse,
  StartOSDiskInfo,
  encodeBase64,
  HttpService,
  isRpcError,
  RpcError,
  RPCOptions,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { ApiService } from './api.service'
import * as jose from 'node-jose'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  constructor(private readonly http: HttpService) {
    super()
  }

  async getStatus() {
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

  async verifyCifs(source: T.VerifyCifsParams) {
    source.path = source.path.replace('/\\/g', '/')
    return this.rpcRequest<StartOSDiskInfo>({
      method: 'setup.cifs.verify',
      params: source,
    })
  }

  async attach(params: T.AttachParams) {
    return this.rpcRequest<T.SetupProgress>({
      method: 'setup.attach',
      params,
    })
  }

  async execute(setupInfo: T.SetupExecuteParams) {
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

  async complete() {
    const res = await this.rpcRequest<T.SetupResult>({
      method: 'setup.complete',
      params: {},
    })

    return {
      ...res,
      rootCa: encodeBase64(res.rootCa),
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
  source: T.BackupTargetFS | null,
): source is T.Cifs & { type: 'cifs' } {
  return !!(source as T.Cifs)?.hostname
}
