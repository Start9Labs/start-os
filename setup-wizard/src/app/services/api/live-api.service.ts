import { Injectable } from '@angular/core'
import { ApiService, DiskInfo, GetStatusRes, RecoveryStatusRes, SetupEmbassyReq, SetupEmbassyRes } from './api.service'
import { HttpService } from './http.service'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {

  constructor (
    private readonly http: HttpService,
  ) { super() }

  // ** UNENCRYPTED **

  async getStatus () {
    return this.http.rpcRequest<GetStatusRes>({
      method: 'setup.status',
      params: { },
    }, false)
  }

  async getDrives () {
    return this.http.rpcRequest<DiskInfo[]>({
      method: 'setup.disk.list',
      params: { },
    }, false)
  }

  async set02XDrive (logicalname) {
    return this.http.rpcRequest<void>({
      method: 'setup.recovery.v2.set',
      params: { logicalname },
    }, false)
  }

  async getRecoveryStatus () {
    return this.http.rpcRequest<RecoveryStatusRes>({
      method: 'setup.recovery.status',
      params: { },
    }, false)
  }

  // ** ENCRYPTED **

  async verifyProductKey () {
    return this.http.rpcRequest<void>({
      method: 'echo',
      params: { 'message': 'hello' },
    })
  }

  async verify03XPassword (logicalname: string, password: string) {
    return this.http.rpcRequest<boolean>({
      method: 'setup.recovery.test-password',
      params: { logicalname, password },
    })
  }

  async importDrive (guid: string) {
    const res = await this.http.rpcRequest<SetupEmbassyRes>({
      method: 'setup.attach',
      params: { guid },
    })

    return {
      ...res,
      'root-ca': btoa(res['root-ca']),
    }
  }

  async setupEmbassy (setupInfo: SetupEmbassyReq) {
    const res = await this.http.rpcRequest<SetupEmbassyRes>({
      method: 'setup.execute',
      params: setupInfo as any,
    })

    return {
      ...res,
      'root-ca': btoa(res['root-ca']),
    }
  }
}
