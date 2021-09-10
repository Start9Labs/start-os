import { Injectable } from '@angular/core'
import { ApiService, EmbassyDrive, RecoveryDrive, SetupEmbassyRes, TransferProgressRes, VerifyProductKeyRes } from './api.service'
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

  async getEmbassyDrives() {
    return this.http.rpcRequest<EmbassyDrive[]>({
      method: 'setup.disk.list',
      params: {}
    })
  }

  async getRecoveryDrives() {
    return this.http.rpcRequest<RecoveryDrive[]>({
      method: 'setup.recovery.list',
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
