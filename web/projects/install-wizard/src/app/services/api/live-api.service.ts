import { Injectable } from '@angular/core'
import {
  HttpService,
  isRpcError,
  RpcError,
  RPCOptions,
} from '@start9labs/shared'
import { ApiService, GetDisksRes, InstallReq } from './api.service'

@Injectable()
export class LiveApiService implements ApiService {
  constructor(private readonly http: HttpService) {}

  async getDisks(): Promise<GetDisksRes> {
    return this.rpcRequest({
      method: 'install.disk.list',
      params: {},
    })
  }

  async install(params: InstallReq): Promise<void> {
    return this.rpcRequest<void>({
      method: 'install.execute',
      params,
    })
  }

  async reboot(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'install.reboot',
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
