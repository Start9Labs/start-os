import { Injectable } from '@angular/core'
import { HttpService } from '../http.service'
import { ApiService, GetErrorRes, GetLogsReq, GetLogsRes } from './api.service'

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
      method: 'diagnostic.forget-disk',
      params: {},
    })
  }

  getLogs(params: GetLogsReq): Promise<GetLogsRes> {
    return this.http.rpcRequest<GetLogsRes>({
      method: 'diagnostic.logs',
      params,
    })
  }
}
