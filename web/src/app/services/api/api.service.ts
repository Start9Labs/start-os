import { Injectable } from '@angular/core'
import { UciFile } from './types'

@Injectable({
  providedIn: 'root',
})
export abstract class ApiService {
  abstract login(params: LoginReq): Promise<null>
  abstract logout(): Promise<null>
  abstract exec(params: ExecReq): Promise<ExecRes>
  abstract getFile(params: GetFileReq): Promise<GetFileRes>
  abstract setFile(params: SetFileReq): Promise<null>
  abstract getUci<T extends Record<string, UciFile<any>>>(
    params: GetUciReq,
  ): Promise<T>
  abstract setUci<T extends string[]>(params: SetUciReq): Promise<SetUciRes<T>>
  abstract systemInfo(): Promise<SystemInfoRes>
  abstract systemNewerVersions(): Promise<VersionInfo[]>
  abstract systemRestart(): Promise<null>
  abstract setPassword(params: SetPasswordReq): Promise<null>
  abstract setPreferences(params: SetPreferencesReq): Promise<null>
  abstract vpnServerList(): Promise<VpnServers>
  abstract vpnServerSet(params: VpnServerSetArgs): Promise<null>
  abstract vpnServerDelete(params: VpnServerDeleteArgs): Promise<null>
  abstract vpnServerPeerAdd(
    params: VpnServerPeerAddArgs,
  ): Promise<VpnServerPeerAddResponse>
  abstract vpnServerPeerDelete(params: VpnServerPeerDeleteArgs): Promise<null>
  abstract wifiGet(): Promise<WifiConfig>
  abstract wifiSet(params: WifiConfig): Promise<null>
  abstract wifiBlackoutGet(): Promise<BlackoutWindow[]>
  abstract wifiBlackoutSet(params: BlackoutWindow[]): Promise<null>
  abstract profilesList(): Promise<ProfileId[]>
  abstract profileGet(params: ProfileIdOpt): Promise<SecurityProfile>
  abstract profileCreate(params: ProfileCreateInput): Promise<ProfileId>
  abstract profileUpdate(params: ProfileUpdateInput): Promise<ProfileId>
  abstract profileDelete(params: ProfileIdOpt): Promise<null>
  abstract checkInitialized(): Promise<CheckInitializedRes>
  abstract setInitialPassword(params: SetInitialPasswordReq): Promise<null>
  abstract setupStatus(): Promise<SetupStatusRes>
  abstract systemFactoryReset(): Promise<null>
  abstract systemLogs(): Promise<LogsResponse>
  abstract devicesList(): Promise<DeviceFromApi[]>
  abstract devicesUpdate(params: DeviceUpdateReq): Promise<null>
  abstract devicesBlock(params: { mac: string }): Promise<null>
  abstract devicesUnblock(params: { mac: string }): Promise<null>
  abstract devicesForget(params: { mac: string }): Promise<null>
  abstract devicesDataUsage(
    params: DeviceDataUsageReq,
  ): Promise<DataUsagePointFromApi[]>
  abstract lanIpv6Get(): Promise<LanIpv6Response>
  abstract lanIpv6Set(params: LanIpv6SetRequest): Promise<null>
}

export type LanIpv6Response = {
  slaac: boolean
  dhcpv6: boolean
  prefix: number
  ip6addr: string | null
  wan_prefix: number
}

export type LanIpv6SetRequest = {
  slaac: boolean
  dhcpv6: boolean
  prefix: number
}

export type LogEntry = { timestamp: string; message: string }
export type LogsResponse = { entries: LogEntry[] }

export type LoginReq = { password: string }

export type ExecReq = {
  command: string
  args: string[]
  timeout: number
}

export type ExecRes = {
  stdout: string
  stderr: string
  exitCode: number
}

export type GetFileReq = {
  path: string
}

export type GetFileRes = {
  contents: string
  modified: string
}

export type SetFileReq = GetFileReq & GetFileRes

export type GetUciReq = {
  names: string[]
}

export type GetUciRes<T> = T

export type SetUciReq = Record<string, UciFile<any>>

export type SetUciRes<T extends string[]> = {
  [K in T[number]]: string
}

export type SystemInfoRes = {
  version: string
  language: string
  date: string
  theme: 'dark' | 'light' | 'system'
  remoteAccess: RemoteAccess
}

export type VersionInfo = {
  version: string
  releaseNotes: string
}

export type SetPasswordReq = {
  oldPassword: string
  newPassword: string
}

export type Theme = 'dark' | 'light' | 'system'

export type RemoteAccess = 'default' | 'never' | 'always'

export type SetPreferencesReq = Partial<{
  language: string
  theme: Theme
  remoteAccess: RemoteAccess
}>

export interface VpnServerPeer {
  name: string
  ip?: string
  public_key?: string
  preshared_key?: string
}

export interface VpnServer {
  profile: string
  label: string
  enabled: boolean
  listen_port: number
  endpoint: string
  public_key: string
  server_address: string
  peers: VpnServerPeer[]
}

export interface VpnServerConfig {
  label: string
  enabled: boolean
  listen_port: number
  endpoint: string
  private_key?: string
}

export interface VpnServers {
  servers: VpnServer[]
}

export interface VpnServerSetArgs {
  profile: string
  config: VpnServerConfig
}

export interface VpnServerDeleteArgs {
  profile: string
}

export interface VpnServerPeerAddArgs {
  profile: string
  peer: VpnServerPeer
}

export interface VpnServerPeerDeleteArgs {
  profile: string
  public_key: string
}

export interface VpnServerPeerAddResponse {
  client_config?: string
  public_key: string
  ip: string
}

export interface WifiRadio {
  band: string
  channel: string
  enabled: boolean
  broadcast: boolean
}

export interface WifiPassword {
  label: string
  profile: WifiProfileId | null
  password: string
}

export interface WifiProfileId {
  fullname: string
  interface: string
  vlan_tag: number
}

export interface WifiConfig {
  ssid: string
  radios: Record<string, WifiRadio>
  passwords: WifiPassword[]
}

export interface BlackoutWindow {
  startTime: string
  endTime: string
  days: [boolean, boolean, boolean, boolean, boolean, boolean, boolean]
}

// Security Profile types
export interface ProfileId {
  fullname: string
  interface: string
  vlan_tag: number
}

export interface ProfileIdOpt {
  fullname?: string
  interface?: string
  vlan_tag?: number
}

export interface SecurityProfile {
  fullname: string
  interface: string
  vlan_tag: number
  gateway_ip: string
  outbound: string // 'wan' for default WAN, or VPN interface name
  lan_access: LanAccess<ProfileId>
  wan_access: WanAccess
  access_to_new_profiles: boolean
  owns_lan: boolean
  dns_override?: string[] // Not yet supported by backend
}

export type LanAccess<Id = ProfileId> =
  | 'ALL'
  | 'SAME_PROFILE'
  | { other_profiles: Id[] }

export type WanAccess =
  | 'ALL'
  | 'NONE'
  | { whitelist: string[] }
  | { blacklist: string[] }

export interface ProfileCreateInput {
  fullname?: string
  interface?: string
  vlan_tag?: number
  gateway_ip: string
  outbound: string
  lan_access: LanAccess<ProfileIdOpt>
  wan_access: WanAccess
  access_to_new_profiles: boolean
  owns_lan: boolean
  dns_override?: string[]
}

export interface ProfileUpdateInput {
  fullname?: string
  interface: string
  vlan_tag: number
  gateway_ip: string
  outbound: string
  lan_access: LanAccess<ProfileIdOpt>
  wan_access: WanAccess
  access_to_new_profiles: boolean
  owns_lan: boolean
  dns_override?: string[]
}
export type CheckInitializedRes = { initialized: boolean }

export type SetInitialPasswordReq = { password: string }

export interface SetupStatusRes {
  setupMode: boolean
  disk: {
    emmcFound: boolean
    hasFirmware: boolean
  }
}

export interface SetupFlashReq {
  mode: 'update' | 'fresh-start'
  password: string
}

export interface SetupFlashEvent {
  phase: 'copying' | 'status' | 'complete' | 'error'
  copied?: number
  total?: number
  message?: string
  step?: number
  totalSteps?: number
}

// Device types (from backend smart endpoints)
export interface DeviceFromApi {
  mac: string
  name: string | null
  hostname: string | null
  status: 'online' | 'offline' | 'blocked'
  connection: string | null
  ipv4: string | null
  ipv6: string | null
  ipv4_static: boolean
  ipv6_static: boolean
  security_profile: string | null
  speed: { up: number; down: number } | null
  data_usage: number | null
}

export interface DeviceUpdateReq {
  mac: string
  name: string
  ipv4_static: boolean
  ipv4: string
  ipv6_static: boolean
  ipv6: string
}

export type DeviceDataUsagePeriod = 'day' | 'week' | 'month' | '3months'

export interface DeviceDataUsageReq {
  mac: string
  period: DeviceDataUsagePeriod
}

export interface DataUsagePointFromApi {
  timestamp: number
  upload: number
  download: number
}
