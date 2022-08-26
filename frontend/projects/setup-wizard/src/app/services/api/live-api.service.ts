import { Injectable } from '@angular/core'
import { HttpService } from '@start9labs/shared'
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
import * as jose from 'jose'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  constructor(
    private readonly unencrypted: HttpService,
    private readonly encrypted: RPCEncryptedService,
  ) {
    super()
  }

  // ** UNENCRYPTED **

  async getStatus() {
    return this.unencrypted.rpcRequest<GetStatusRes>({
      method: 'setup.status',
      params: {},
    })
  }

  /**
   * We want to update the secret, which means that we will call in clearnet the
   * getSecret, and all the information is never in the clear, and only public
   *  information is sent across the network. We don't want to expose that we do
   * this wil all public/private key, which means that there is no information loss
   * through the network.
   */
  async getSecret() {
    const { privateKey, publicKey } = await jose.generateKeyPair('ECDH-ES', {
      extractable: true,
    })
    console.log({ publicKey: await jose.exportJWK(publicKey) })
    const response: string = await this.unencrypted.rpcRequest({
      method: 'setup.get-secret',
      params: { pubkey: await jose.exportJWK(publicKey) },
    })

    const { plaintext } = await jose.compactDecrypt(response, privateKey)
    const decoded = new TextDecoder().decode(plaintext)
    console.log({ decoded })

    return decoded
  }

  async getDrives() {
    return this.unencrypted.rpcRequest<DiskListResponse>({
      method: 'setup.disk.list',
      params: {},
    })
  }

  async set02XDrive(logicalname: string) {
    return this.unencrypted.rpcRequest<void>({
      method: 'setup.recovery.v2.set',
      params: { logicalname },
    })
  }

  async getRecoveryStatus() {
    return this.unencrypted.rpcRequest<RecoveryStatusRes>({
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
}

function isCifsSource(
  source: CifsRecoverySource | DiskRecoverySource | null,
): source is CifsRecoverySource {
  return !!(source as CifsRecoverySource)?.hostname
}
