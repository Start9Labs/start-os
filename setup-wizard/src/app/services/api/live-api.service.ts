import { Injectable } from '@angular/core'
import { ApiService, DiskInfo, SetupEmbassyRes, TransferProgressRes, VerifyProductKeyRes } from './api.service'
import { HttpService } from './http.service'

@Injectable({
  providedIn: 'root'
})
export class LiveApiService extends ApiService {

  constructor(
    private readonly http: HttpService
  ) { super() }

  async verifyProductKey() {
    return this.http.rpcRequest<VerifyProductKeyRes>({
      method: 'setup.status',
      params: {}
    })
  }

  async getDataTransferProgress() {
    return this.http.rpcRequest<TransferProgressRes>({
      method: 'setup.recovery.status',
      params: {}
    })
  }

  async getDrives() {
    return this.http.rpcRequest<DiskInfo[]>({
      method: 'setup.disk.list',
      params: {}
    })
  }

  async verifyRecoveryPassword(logicalname, password) {
    return this.http.rpcRequest<boolean>({
      method: 'setup.recovery.test-password',
      params: {logicalname, password}
    })
  }

  async setupEmbassy (setupInfo) {
    return this.http.rpcRequest<SetupEmbassyRes>({
      method: 'setup.execute',
      params: setupInfo
    })
  }
}
