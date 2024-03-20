import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { Url } from '@start9labs/shared'
import { Manifest } from '@start9labs/marketplace'
import { BackupJob, ServerNotifications } from '../api/api.types'
import { customSmtp } from '@start9labs/start-sdk/lib/config/configConstants'
import { types } from '@start9labs/start-sdk'
type ServiceInterfaceWithHostInfo = types.ServiceInterfaceWithHostInfo

export interface DataModel {
  'server-info': ServerInfo
  'package-data': { [id: string]: PackageDataEntry }
  ui: UIData
}

export interface UIData {
  name: string | null
  'ack-welcome': string // emver
  marketplace: UIMarketplaceData
  gaming: {
    snake: {
      'high-score': number
    }
  }
  'ack-instructions': Record<string, boolean>
  theme: string
  widgets: readonly Widget[]
  desktop: readonly string[]
}

export interface Widget {
  id: string
  meta: {
    name: string
    width: number
    height: number
    mobileWidth: number
    mobileHeight: number
  }
  url?: string
  settings?: string
}

export interface UIMarketplaceData {
  'selected-url': string
  'known-hosts': {
    'https://registry.start9.com/': UIStore
    'https://community-registry.start9.com/': UIStore
    [url: string]: UIStore
  }
}

export interface UIStore {
  name?: string
}

export interface ServerInfo {
  id: string
  version: string
  country: string
  ui: HostnameInfo[]
  network: NetworkInfo
  'last-backup': string | null
  unreadNotifications: {
    count: number
    recent: ServerNotifications
  }
  'status-info': ServerStatusInfo
  'eos-version-compat': string
  pubkey: string
  'ca-fingerprint': string
  'ntp-synced': boolean
  smtp: typeof customSmtp.validator._TYPE
  'password-hash': string
  platform: string
}

export type NetworkInfo = {
  wifi: WiFiInfo
  start9ToSubdomain: Omit<Domain, 'provider'> | null
  domains: Domain[]
  wanConfig: {
    upnp: boolean
    forwards: PortForward[]
  }
  proxies: Proxy[]
  outboundProxy: OsOutboundProxy
  primaryProxies: {
    inbound: string | null
    outbound: string | null
  }
}

export type DomainInfo = {
  domain: string
  subdomain: string | null
}

export type InboundProxy = { proxyId: string } | 'primary' | null
export type OsOutboundProxy = InboundProxy
export type ServiceOutboundProxy = OsOutboundProxy | 'mirror'

export type PortForward = {
  assigned: number
  override: number | null
  target: number
  error: string | null
}

export type WiFiInfo = {
  enabled: boolean
  lastRegion: string | null
}

export type Domain = {
  value: string
  createdAt: string
  provider: string
  networkStrategy: NetworkStrategy
  usedBy: {
    service: { id: string | null; title: string } // null means startos
    interfaces: { id: string | null; title: string }[] // null means startos
  }[]
}

export type NetworkStrategy =
  | { proxyId: string | null } // null means system primary
  | { ipStrategy: 'ipv4' | 'ipv6' | 'dualstack' }

export type Proxy = {
  id: string
  name: string
  createdAt: string
  type: 'outbound' | 'inbound-outbound' | 'vlan' | { error: string }
  endpoint: string
  // below is overlay only
  usedBy: {
    services: { id: string | null; title: string }[] // implies outbound - null means startos
    domains: string[] // implies inbound
  }
  primaryInbound: boolean
  primaryOutbound: boolean
}

export interface IpInfo {
  [iface: string]: {
    wireless: boolean
    ipv4: string | null
    ipv6: string | null
  }
}

export interface ServerStatusInfo {
  'current-backup': null | {
    job: BackupJob
    'backup-progress': {
      [packageId: string]: {
        complete: boolean
      }
    }
  }
  updated: boolean
  'update-progress': { size: number | null; downloaded: number } | null
  restarting: boolean
  'shutting-down': boolean
}

export enum ServerStatus {
  Running = 'running',
  Updated = 'updated',
  BackingUp = 'backing-up',
}

export type PackageDataEntry<T extends StateInfo = StateInfo> = {
  'state-info': T
  icon: Url
  status: Status
  'last-backup': string | null
  'current-dependents': { [id: string]: CurrentDependencyInfo }
  'current-dependencies': { [id: string]: CurrentDependencyInfo }
  'dependency-info': {
    [id: string]: {
      title: string
      icon: Url
    }
  }
  'service-interfaces': Record<string, ServiceInterfaceWithHostInfo>
  'marketplace-url': string | null
  'developer-key': string
  'has-config': boolean
  outboundProxy: ServiceOutboundProxy
}

export type StateInfo = InstalledState | InstallingState | UpdatingState

export type InstalledState = {
  state: PackageState.Installed | PackageState.Removing
  manifest: Manifest
}

export type InstallingState = {
  state: PackageState.Installing | PackageState.Restoring
  'installing-info': InstallingInfo
}

export type UpdatingState = {
  state: PackageState.Updating
  'installing-info': InstallingInfo
  manifest: Manifest
}

export enum PackageState {
  Installing = 'installing',
  Installed = 'installed',
  Updating = 'updating',
  Removing = 'removing',
  Restoring = 'restoring',
}

export interface CurrentDependencyInfo {
  'health-checks': string[] // array of health check IDs
}

export interface Action {
  name: string
  description: string
  warning: string | null
  disabled: string | null
  'input-spec': InputSpec | null
  group: string | null
}

export interface Status {
  configured: boolean
  main: MainStatus
  'dependency-config-errors': { [id: string]: string | null }
}

export type MainStatus =
  | MainStatusStopped
  | MainStatusStopping
  | MainStatusStarting
  | MainStatusRunning
  | MainStatusBackingUp
  | MainStatusRestarting
  | MainStatusConfiguring

export interface MainStatusStopped {
  status: PackageMainStatus.Stopped
}

export interface MainStatusStopping {
  status: PackageMainStatus.Stopping
  timeout: string
}

export interface MainStatusStarting {
  status: PackageMainStatus.Starting
}

export interface MainStatusRunning {
  status: PackageMainStatus.Running
  started: string // UTC date string
  health: { [id: string]: HealthCheckResult }
}

export interface MainStatusBackingUp {
  status: PackageMainStatus.BackingUp
}

export interface MainStatusRestarting {
  status: PackageMainStatus.Restarting
}

export interface MainStatusConfiguring {
  status: PackageMainStatus.Configuring
}

export enum PackageMainStatus {
  Starting = 'starting',
  Running = 'running',
  Stopping = 'stopping',
  Stopped = 'stopped',
  BackingUp = 'backing-up',
  Restarting = 'restarting',
  Configuring = 'configuring',
}

export type HealthCheckResult = { name: string } & (
  | HealthCheckResultStarting
  | HealthCheckResultLoading
  | HealthCheckResultDisabled
  | HealthCheckResultSuccess
  | HealthCheckResultFailure
)

export enum HealthResult {
  Starting = 'starting',
  Loading = 'loading',
  Disabled = 'disabled',
  Success = 'success',
  Failure = 'failure',
}

export interface HealthCheckResultStarting {
  result: HealthResult.Starting
}

export interface HealthCheckResultDisabled {
  result: HealthResult.Disabled
  reason: string
}

export interface HealthCheckResultSuccess {
  result: HealthResult.Success
  message: string
}

export interface HealthCheckResultLoading {
  result: HealthResult.Loading
  message: string
}

export interface HealthCheckResultFailure {
  result: HealthResult.Failure
  error: string
}

export type InstallingInfo = {
  progress: FullProgress
  'new-manifest': Manifest
}

export type FullProgress = {
  overall: Progress
  phases: { name: string; progress: Progress }[]
}
export type Progress = boolean | { done: number; total: number | null } // false means indeterminate. true means complete
