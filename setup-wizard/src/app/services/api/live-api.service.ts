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
      method: 'verifyProductKey',
      params: {}
    })
  }

  async getDataTransferProgress() {
    return this.http.rpcRequest<TransferProgressRes>({
      method: 'getDataTransferProgress',
      params: {}
    })
  }

  async getEmbassyDrives() {
    return this.http.rpcRequest<EmbassyDrive[]>({
      method: 'getEmbassyDrives',
      params: {}
    })
  }

  async getRecoveryDrives() {
    return this.http.rpcRequest<RecoveryDrive[]>({
      method: 'getRecoveryDrives',
      params: {}
    })
  }

  async verifyRecoveryPassword(logicalname, password) {
    return this.http.rpcRequest<boolean>({
      method: 'verifyRecoveryPassword',
      params: {logicalname, password}
    })
  }

  async setupEmbassy (setupInfo) {
    return this.http.rpcRequest<SetupEmbassyRes>({
      method: 'setupEmbassy',
      params: setupInfo
    })
  }
}
