import { Injectable } from '@angular/core'
import {
  HttpService,
  isRpcError,
  RpcError,
  RPCOptions,
} from '@start9labs/shared'
import { ApiService, GetErrorRes } from './api.service'
import { LogsRes, ServerLogsReq } from '@start9labs/shared'

@Injectable()
export class LiveApiService implements ApiService {
  constructor(private readonly http: HttpService) {}

  async getError(): Promise<GetErrorRes> {
    return this.rpcRequest<GetErrorRes>({
      method: 'diagnostic.error',
      params: {},
    })
  }

  async restart(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.restart',
      params: {},
    })
  }

  async forgetDrive(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.disk.forget',
      params: {},
    })
  }

  async repairDisk(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.disk.repair',
      params: {},
    })
  }

  async systemRebuild(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.rebuild',
      params: {},
    })
  }

  async getLogs(params: ServerLogsReq): Promise<LogsRes> {
    return this.rpcRequest<LogsRes>({
      method: 'diagnostic.logs',
      params,
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
