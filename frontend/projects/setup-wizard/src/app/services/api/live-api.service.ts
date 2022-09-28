import { Injectable } from '@angular/core'
import {
  encodeBase64,
  HttpService,
  isRpcError,
  RpcError,
  RPCOptions,
} from '@start9labs/shared'
import {
  ApiService,
  CifsRecoverySource,
  DiskListResponse,
  DiskRecoverySource,
  EmbassyOSRecoveryInfo,
  GetStatusRes,
  ImportDriveReq,
  RecoveryStatusRes,
  SetupEmbassyReq,
  SetupEmbassyRes,
} from './api.service'
import * as jose from 'node-jose'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  constructor(private readonly http: HttpService) {
    super()
  }

  async getStatus() {
    return this.rpcRequest<GetStatusRes>({
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

  async set02XDrive(logicalname: string) {
    return this.rpcRequest<void>({
      method: 'setup.recovery.v2.set',
      params: { logicalname },
    })
  }

  async getRecoveryStatus() {
    return this.rpcRequest<RecoveryStatusRes>({
      method: 'setup.recovery.status',
      params: {},
    })
  }

  async verifyCifs(source: CifsRecoverySource) {
    source.path = source.path.replace('/\\/g', '/')
    return this.rpcRequest<EmbassyOSRecoveryInfo>({
      method: 'setup.cifs.verify',
      params: source,
    })
  }

  async importDrive(params: ImportDriveReq) {
    const res = await this.rpcRequest<SetupEmbassyRes>({
      method: 'setup.attach',
      params,
    })

    return {
      ...res,
      'root-ca': encodeBase64(res['root-ca']),
    }
  }

  async setupEmbassy(setupInfo: SetupEmbassyReq) {
    if (isCifsSource(setupInfo['recovery-source'])) {
      setupInfo['recovery-source'].path = setupInfo[
        'recovery-source'
      ].path.replace('/\\/g', '/')
    }

    const res = await this.rpcRequest<SetupEmbassyRes>({
      method: 'setup.execute',
      params: setupInfo,
    })

    return {
      ...res,
      'root-ca': encodeBase64(res['root-ca']),
    }
  }

  async setupComplete() {
    const res = await this.rpcRequest<SetupEmbassyRes>({
      method: 'setup.complete',
      params: {},
    })

    return {
      ...res,
      'root-ca': encodeBase64(res['root-ca']),
    }
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
