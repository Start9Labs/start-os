import { Injectable } from '@angular/core'
import {
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
import { RPCEncryptedService } from '../rpc-encrypted.service'
import * as jose from 'node-jose'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService implements ApiService {
  constructor(
    private readonly unencrypted: HttpService,
    private readonly encrypted: RPCEncryptedService,
  ) {}

  // ** UNENCRYPTED **

  async getStatus() {
    return this.rpcRequest<GetStatusRes>({
      method: 'setup.status',
      params: {},
    })
  }

  /**
   * We want to update the secret, which means that we will call in clearnet the
   * getSecret, and all the information is never in the clear, and only public
   * information is sent across the network. We don't want to expose that we do
   * this wil all public/private key, which means that there is no information loss
   * through the network.
   */
  async getSecret() {
    const keystore = jose.JWK.createKeyStore()
    const key = await keystore.generate('EC', 'P-256')
    // const { privateKey, publicKey } =

    // jose.generateKeyPair('ECDH-ES', {
    //   extractable: true,
    // })
    console.log({ publicKey: key.toJSON() })
    const response: string = await this.rpcRequest({
      method: 'setup.get-secret',
      params: { pubkey: key.toJSON() },
    })

    // const { plaintext } = await jose.compactDecrypt(response, privateKey)
    const decrypted = await jose.JWE.createDecrypt(key).decrypt(response)
    const decoded = new TextDecoder().decode(decrypted.plaintext)
    console.log({ decoded })

    return decoded
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

  // ** ENCRYPTED **

  async verifyCifs(source: CifsRecoverySource) {
    source.path = source.path.replace('/\\/g', '/')
    return this.encrypted.rpcRequest<EmbassyOSRecoveryInfo>({
      method: 'setup.cifs.verify',
      params: source,
    })
  }

  async importDrive(params: ImportDriveReq) {
    const res = await this.encrypted.rpcRequest<SetupEmbassyRes>({
      method: 'setup.attach',
      params,
    })

    return {
      ...res,
      'root-ca': btoa(res['root-ca']),
    }
  }

  async setupEmbassy(setupInfo: SetupEmbassyReq) {
    if (isCifsSource(setupInfo['recovery-source'])) {
      setupInfo['recovery-source'].path = setupInfo[
        'recovery-source'
      ].path.replace('/\\/g', '/')
    }

    const res = await this.encrypted.rpcRequest<SetupEmbassyRes>({
      method: 'setup.execute',
      params: setupInfo,
    })

    return {
      ...res,
      'root-ca': btoa(res['root-ca']),
    }
  }

  async setupComplete() {
    const res = await this.encrypted.rpcRequest<SetupEmbassyRes>({
      method: 'setup.complete',
      params: {},
    })

    return {
      ...res,
      'root-ca': btoa(res['root-ca']),
    }
  }

  private async rpcRequest<T>(opts: RPCOptions): Promise<T> {
    const res = await this.unencrypted.rpcRequest<T>(opts)

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
