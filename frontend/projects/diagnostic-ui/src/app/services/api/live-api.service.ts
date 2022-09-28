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

  getError(): Promise<GetErrorRes> {
    return this.rpcRequest<GetErrorRes>({
      method: 'diagnostic.error',
      params: {},
    })
  }

  restart(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.restart',
      params: {},
    })
  }

  forgetDrive(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.disk.forget',
      params: {},
    })
  }

  repairDisk(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.disk.repair',
      params: {},
    })
  }

  getLogs(params: ServerLogsReq): Promise<LogsRes> {
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
