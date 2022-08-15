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
    return this.unencrypted
      .rpcRequest<GetStatusRes>({
        method: 'setup.status',
        params: {},
      })
      .then(res => res.result)
  }

  async getDrives() {
    return this.unencrypted
      .rpcRequest<DiskListResponse>({
        method: 'setup.disk.list',
        params: {},
      })
      .then(res => res.result)
  }

  async set02XDrive(logicalname: string) {
    return this.unencrypted
      .rpcRequest<void>({
        method: 'setup.recovery.v2.set',
        params: { logicalname },
      })
      .then(res => res.result)
  }

  async getRecoveryStatus() {
    return this.unencrypted
      .rpcRequest<RecoveryStatusRes>({
        method: 'setup.recovery.status',
        params: {},
      })
      .then(res => res.result)
  }

  // ** ENCRYPTED **

  async verifyCifs(source: CifsRecoverySource) {
    source.path = source.path.replace('/\\/g', '/')
    return this.encrypted
      .rpcRequest<EmbassyOSRecoveryInfo>({
        method: 'setup.cifs.verify',
        params: source,
      })
      .then(res => res.result)
  }

  async verifyProductKey() {
    return this.encrypted
      .rpcRequest<void>({
        method: 'echo',
        params: { message: 'hello' },
      })
      .then(res => res.result)
  }

  async importDrive(params: ImportDriveReq) {
    const res = await this.encrypted
      .rpcRequest<SetupEmbassyRes>({
        method: 'setup.attach',
        params,
      })
      .then(res => res.result)

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

    const result = res.result

    return {
      ...result,
      'root-ca': btoa(result['root-ca']),
    }
  }

  async setupComplete() {
    const res = await this.encrypted.rpcRequest<SetupEmbassyRes>({
      method: 'setup.complete',
      params: {},
    })

    const result = res.result

    return {
      ...result,
      'root-ca': btoa(result['root-ca']),
    }
  }
}

function isCifsSource(
  source: CifsRecoverySource | DiskRecoverySource | null,
): source is CifsRecoverySource {
  return !!(source as CifsRecoverySource)?.hostname
}
