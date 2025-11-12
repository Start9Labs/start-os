import { Injectable, inject } from '@angular/core'
import {
  ApiService,
  ExecReq,
  ExecRes,
  GetFileReq,
  GetFileRes,
  GetUciReq,
  GetUciRes,
  LoginReq,
  SetUciReq,
} from './api.service'
import { RpcService } from '../rpc.service'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  private readonly rpc = inject(RpcService)

  constructor() {
    super()
  }

  // auth

  async login(params: LoginReq): Promise<null> {
    return this.rpc.request({ method: 'auth.login', params })
  }

  async logout(): Promise<null> {
    return this.rpc.request({ method: 'auth.logout', params: {} })
  }

  // exec
  async exec(params: ExecReq): Promise<ExecRes> {
    return this.rpc.request({ method: 'exec', params })
  }

  // file
  async getFile(params: GetFileReq): Promise<GetFileRes> {
    return this.rpc.request({ method: 'file.get', params })
  }

  async setFile(params: GetFileRes): Promise<null> {
    return this.rpc.request({ method: 'file.set', params })
  }

  // uci
  async getUci(params: GetUciReq): Promise<GetUciRes> {
    return this.rpc.request({ method: 'uci.get', params })
  }

  async setUci(params: SetUciReq): Promise<null> {
    return this.rpc.request({ method: 'uci.set', params })
  }
}
