import { Injectable } from '@angular/core'
import { HttpService } from '@start9labs/shared'
import { ApiService, GetErrorRes } from './api.service'
import { LogsRes, ServerLogsReq } from '@start9labs/shared'

@Injectable()
export class LiveApiService extends ApiService {
  constructor(private readonly http: HttpService) {
    super()
  }

  getError(): Promise<GetErrorRes> {
    return this.http.rpcRequest<GetErrorRes>({
      method: 'diagnostic.error',
      params: {},
    })
  }

  restart(): Promise<void> {
    return this.http.rpcRequest<void>({
      method: 'diagnostic.restart',
      params: {},
    })
  }

  forgetDrive(): Promise<void> {
    return this.http.rpcRequest<void>({
      method: 'diagnostic.disk.forget',
      params: {},
    })
  }

  repairDisk(): Promise<void> {
    return this.http.rpcRequest<void>({
      method: 'diagnostic.disk.repair',
      params: {},
    })
  }

  getLogs(params: ServerLogsReq): Promise<LogsRes> {
    return this.http.rpcRequest<LogsRes>({
      method: 'diagnostic.logs',
      params,
    })
  }
}
