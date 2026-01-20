import { Injectable, inject } from '@angular/core'
import {
  ApiService,
  ExecReq,
  ExecRes,
  GetFileReq,
  GetFileRes,
  GetUciReq,
  LoginReq,
  SystemInfoRes,
  SetFileReq,
  SetUciReq,
  SetUciRes,
  VersionInfo,
  SetPasswordReq,
  SetPreferencesReq,
} from './api.service'
import { RpcService } from '../rpc.service'
import { UciFile } from './types'

@Injectable({
  providedIn: 'root',
})
export class LiveApiService extends ApiService {
  private readonly rpc = inject(RpcService)

  async login(params: LoginReq): Promise<null> {
    return this.rpc.request({ method: 'auth.login', params })
  }

  async logout(): Promise<null> {
    return this.rpc.request({ method: 'auth.logout', params: {} })
  }

  async exec(params: ExecReq): Promise<ExecRes> {
    return this.rpc.request({ method: 'exec', params })
  }

  async getFile(params: GetFileReq): Promise<GetFileRes> {
    return this.rpc.request({ method: 'file.get', params })
  }

  async setFile(params: SetFileReq): Promise<null> {
    return this.rpc.request({ method: 'file.set', params })
  }

  async getUci<T extends Record<string, UciFile<any>>>(
    params: GetUciReq,
  ): Promise<T> {
    return this.rpc.request({ method: 'uci.get', params })
  }

  async setUci<T extends string[]>(params: SetUciReq): Promise<SetUciRes<T>> {
    return this.rpc.request({ method: 'uci.set', params })
  }

  async systemInfo(): Promise<SystemInfoRes> {
    return this.rpc.request({ method: 'system.info', params: {} })
  }

  async systemNewerVersions(): Promise<VersionInfo[]> {
    return this.rpc.request({ method: 'system.newer-versions', params: {} })
  }

  async systemRestart(): Promise<null> {
    await this.exec({ command: 'reboot', args: [], timeout: 5000 })
    return null
  }

  async setPassword(params: SetPasswordReq): Promise<null> {
    return this.rpc.request({ method: 'auth.set-password', params })
  }

  async setPreferences(params: SetPreferencesReq): Promise<null> {
    return this.rpc.request({ method: 'system.set-preferences', params })
  }
}
