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
  WanIpv4Response,
  WanIpv4SetRequest,
  WanIpv6Response,
  WanIpv6SetRequest,
  WanMacResponse,
  WanMacSetRequest,
  WanDnsResponse,
  WanDnsSetRequest,
  WanDdnsResponse,
  WanDdnsSetRequest,
  PublishedPortFromApi,
  PublishedPortsSetRequest,
  OutboundVpn,
  OutboundVpnCreateRequest,
  OutboundVpnCreateResponse,
  OutboundVpnUpdateRequest,
  OutboundVpnDeleteRequest,
  OutboundVpnSetEnabledRequest,
  EthernetConfig,
  EthernetSetConfig,
  SshKeyFromApi,
  SshKeysAddRequest,
  SshKeysDeleteRequest,
  ActivityListParams,
  ActivityListResponse,
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
    return this.rpc.request({ method: 'system.restart', params: {} })
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

  async wanIpv4Get(): Promise<WanIpv4Response> {
    return this.rpc.request({ method: 'wan.ipv4-get', params: {} })
  }

  async wanIpv4Set(params: WanIpv4SetRequest): Promise<null> {
    return this.rpc.request({ method: 'wan.ipv4-set', params })
  }

  async wanIpv6Get(): Promise<WanIpv6Response> {
    return this.rpc.request({ method: 'wan.ipv6-get', params: {} })
  }

  async wanIpv6Set(params: WanIpv6SetRequest): Promise<null> {
    return this.rpc.request({ method: 'wan.ipv6-set', params })
  }

  async wanMacGet(): Promise<WanMacResponse> {
    return this.rpc.request({ method: 'wan.mac-get', params: {} })
  }

  async wanMacSet(params: WanMacSetRequest): Promise<null> {
    return this.rpc.request({ method: 'wan.mac-set', params })
  }

  async wanDnsGet(): Promise<WanDnsResponse> {
    return this.rpc.request({ method: 'wan.dns-get', params: {} })
  }

  async wanDnsSet(params: WanDnsSetRequest): Promise<null> {
    return this.rpc.request({ method: 'wan.dns-set', params })
  }

  async wanDdnsGet(): Promise<WanDdnsResponse> {
    return this.rpc.request({ method: 'wan.ddns-get', params: {} })
  }

  async wanDdnsSet(params: WanDdnsSetRequest): Promise<null> {
    return this.rpc.request({ method: 'wan.ddns-set', params })
  }

  async publishedPortsList(): Promise<PublishedPortFromApi[]> {
    return this.rpc.request({ method: 'published-ports.list', params: {} })
  }

  async publishedPortsSet(params: PublishedPortsSetRequest): Promise<null> {
    return this.rpc.request({ method: 'published-ports.set', params })
  }

  async vpnClientList(): Promise<OutboundVpn[]> {
    return this.rpc.request({ method: 'vpn-client.list', params: {} })
  }

  async vpnClientCreate(
    params: OutboundVpnCreateRequest,
  ): Promise<OutboundVpnCreateResponse> {
    return this.rpc.request({ method: 'vpn-client.create', params })
  }

  async vpnClientUpdate(params: OutboundVpnUpdateRequest): Promise<null> {
    return this.rpc.request({ method: 'vpn-client.update', params })
  }

  async vpnClientDelete(params: OutboundVpnDeleteRequest): Promise<null> {
    return this.rpc.request({ method: 'vpn-client.delete', params })
  }

  async vpnClientSetEnabled(
    params: OutboundVpnSetEnabledRequest,
  ): Promise<null> {
    return this.rpc.request({ method: 'vpn-client.set-enabled', params })
  }

  async ethernetGet(): Promise<EthernetConfig> {
    return this.rpc.request({ method: 'ethernet.get', params: {} })
  }

  async ethernetSet(params: EthernetSetConfig): Promise<null> {
    return this.rpc.request({ method: 'ethernet.set', params })
  }

  async sshKeysList(): Promise<SshKeyFromApi[]> {
    return this.rpc.request({ method: 'ssh-keys.list', params: {} })
  }

  async sshKeysAdd(params: SshKeysAddRequest): Promise<SshKeyFromApi> {
    return this.rpc.request({ method: 'ssh-keys.add', params })
  }

  async sshKeysDelete(params: SshKeysDeleteRequest): Promise<null> {
    return this.rpc.request({ method: 'ssh-keys.delete', params })
  }

  async activityList(
    params: ActivityListParams = {},
  ): Promise<ActivityListResponse> {
    return this.rpc.request({ method: 'activity.list', params })
  }

  async activityDelete(params: { id: string }): Promise<null> {
    return this.rpc.request({ method: 'activity.delete', params })
  }

  async activityClear(): Promise<null> {
    return this.rpc.request({ method: 'activity.clear', params: {} })
  }
}
