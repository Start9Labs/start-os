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
  VpnServerDeleteArgs,
  VpnServerPeerAddArgs,
  VpnServerPeerAddResponse,
  VpnServerPeerDeleteArgs,
  VpnServerSetArgs,
  VpnServers,
  WifiConfig,
  BlackoutWindow,
  ProfileId,
  ProfileIdOpt,
  SecurityProfile,
  ProfileCreateInput,
  ProfileUpdateInput,
  CheckInitializedRes,
  SetInitialPasswordReq,
  SetupStatusRes,
  LogsResponse,
  DeviceFromApi,
  DeviceUpdateReq,
  DeviceDataUsageReq,
  DataUsagePointFromApi,
  LanIpv4Response,
  LanIpv4SetRequest,
  LanIpv6Response,
  LanIpv6SetRequest,
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

  async vpnServerList(): Promise<VpnServers> {
    return this.rpc.request({ method: 'vpn-server.list', params: {} })
  }

  async vpnServerSet(params: VpnServerSetArgs): Promise<null> {
    return this.rpc.request({ method: 'vpn-server.set', params })
  }

  async vpnServerDelete(params: VpnServerDeleteArgs): Promise<null> {
    return this.rpc.request({ method: 'vpn-server.delete', params })
  }

  async vpnServerPeerAdd(
    params: VpnServerPeerAddArgs,
  ): Promise<VpnServerPeerAddResponse> {
    return this.rpc.request({ method: 'vpn-server.peer-add', params })
  }

  async vpnServerPeerDelete(params: VpnServerPeerDeleteArgs): Promise<null> {
    return this.rpc.request({ method: 'vpn-server.peer-delete', params })
  }

  async wifiGet(): Promise<WifiConfig> {
    return this.rpc.request({ method: 'wifi.get', params: {} })
  }

  async wifiSet(params: WifiConfig): Promise<null> {
    return this.rpc.request({ method: 'wifi.set', params })
  }

  async wifiBlackoutGet(): Promise<BlackoutWindow[]> {
    return this.rpc.request({ method: 'wifi.blackout-get', params: {} })
  }

  async wifiBlackoutSet(params: BlackoutWindow[]): Promise<null> {
    return this.rpc.request({
      method: 'wifi.blackout-set',
      params: { windows: params },
    })
  }

  async profilesList(): Promise<ProfileId[]> {
    return this.rpc.request({ method: 'profiles.list', params: {} })
  }

  async profileGet(params: ProfileIdOpt): Promise<SecurityProfile> {
    return this.rpc.request({ method: 'profiles.get', params })
  }

  async profileCreate(params: ProfileCreateInput): Promise<ProfileId> {
    return this.rpc.request({ method: 'profiles.create', params })
  }

  async profileUpdate(params: ProfileUpdateInput): Promise<ProfileId> {
    return this.rpc.request({ method: 'profiles.set', params })
  }

  async profileDelete(params: ProfileIdOpt): Promise<null> {
    return this.rpc.request({ method: 'profiles.delete', params })
  }

  async checkInitialized(): Promise<CheckInitializedRes> {
    return this.rpc.request({
      method: 'auth.check-initialized',
      params: {},
    })
  }

  async setInitialPassword(params: SetInitialPasswordReq): Promise<null> {
    return this.rpc.request({
      method: 'auth.set-initial-password',
      params,
    })
  }

  async setupStatus(): Promise<SetupStatusRes> {
    return this.rpc.request({
      method: 'setup.status',
      params: {},
    })
  }

  async systemFactoryReset(): Promise<null> {
    return this.rpc.request({
      method: 'system.factory-reset',
      params: {},
    })
  }

  async systemLogs(): Promise<LogsResponse> {
    return this.rpc.request({ method: 'system.logs', params: {} })
  }

  async devicesList(): Promise<DeviceFromApi[]> {
    return this.rpc.request({ method: 'devices.list', params: {} })
  }

  async devicesUpdate(params: DeviceUpdateReq): Promise<null> {
    return this.rpc.request({ method: 'devices.update', params })
  }

  async devicesBlock(params: { mac: string }): Promise<null> {
    return this.rpc.request({ method: 'devices.block', params })
  }

  async devicesUnblock(params: { mac: string }): Promise<null> {
    return this.rpc.request({ method: 'devices.unblock', params })
  }

  async devicesForget(params: { mac: string }): Promise<null> {
    return this.rpc.request({ method: 'devices.forget', params })
  }

  async devicesDataUsage(
    params: DeviceDataUsageReq,
  ): Promise<DataUsagePointFromApi[]> {
    return this.rpc.request({ method: 'devices.data-usage', params })
  }

  async lanIpv4Get(): Promise<LanIpv4Response> {
    return this.rpc.request({ method: 'lan.ipv4-get', params: {} })
  }

  async lanIpv4Set(params: LanIpv4SetRequest): Promise<null> {
    return this.rpc.request({ method: 'lan.ipv4-set', params })
  }

  async lanIpv6Get(): Promise<LanIpv6Response> {
    return this.rpc.request({ method: 'lan.ipv6-get', params: {} })
  }

  async lanIpv6Set(params: LanIpv6SetRequest): Promise<null> {
    return this.rpc.request({ method: 'lan.ipv6-set', params })
  }
}
