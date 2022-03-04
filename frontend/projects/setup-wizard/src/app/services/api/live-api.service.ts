import { Injectable } from '@angular/core'
import {
  ApiService,
  CifsRecoverySource,
  DiskInfo,
  DiskListResponse,
  DiskRecoverySource,
  EmbassyOSRecoveryInfo,
  GetStatusRes,
  RecoveryStatusRes,
  SetupEmbassyReq,
  SetupEmbassyRes,
} from './api.service'
import { HttpService } from './http.service'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  constructor(private readonly http: HttpService) {
    super()
  }

  // ** UNENCRYPTED **

  async getStatus() {
    return this.http.rpcRequest<GetStatusRes>(
      {
        method: 'setup.status',
        params: {},
      },
      false,
    )
  }

  async getDrives() {
    return this.http.rpcRequest<DiskListResponse>(
      {
        method: 'setup.disk.list',
        params: {},
      },
      false,
    )
  }

  async set02XDrive(logicalname) {
    return this.http.rpcRequest<void>(
      {
        method: 'setup.recovery.v2.set',
        params: { logicalname },
      },
      false,
    )
  }

  async getRecoveryStatus() {
    return this.http.rpcRequest<RecoveryStatusRes>(
      {
        method: 'setup.recovery.status',
        params: {},
      },
      false,
    )
  }

  // ** ENCRYPTED **

  async verifyCifs(source: CifsRecoverySource) {
    source.path = source.path.replace('/\\/g', '/')
    return this.http.rpcRequest<EmbassyOSRecoveryInfo>({
      method: 'setup.cifs.verify',
      params: source as any,
    })
  }

  async verifyProductKey() {
    return this.http.rpcRequest<void>({
      method: 'echo',
      params: { message: 'hello' },
    })
  }

  async importDrive(guid: string) {
    const res = await this.http.rpcRequest<SetupEmbassyRes>({
      method: 'setup.attach',
      params: { guid },
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

    const res = await this.http.rpcRequest<SetupEmbassyRes>({
      method: 'setup.execute',
      params: setupInfo as any,
    })

    return {
      ...res,
      'root-ca': btoa(res['root-ca']),
    }
  }

  async setupComplete() {
    const res = await this.http.rpcRequest<SetupEmbassyRes>({
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
  source: CifsRecoverySource | DiskRecoverySource | undefined,
): source is CifsRecoverySource {
  return !!(source as CifsRecoverySource)?.hostname
}
