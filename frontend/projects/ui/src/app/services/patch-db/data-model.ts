import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { Url } from '@start9labs/shared'
import { Manifest } from '@start9labs/marketplace'
import { BackupJob } from '../api/api.types'
import { customSmtp } from '@start9labs/start-sdk/lib/config/configConstants'
import { NetworkInterfaceType } from '@start9labs/start-sdk/lib/util/utils'

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
  ui: AddressInfo
  network: NetworkInfo
  'last-backup': string | null
  'unread-notification-count': number
  'status-info': ServerStatusInfo
  'eos-version-compat': string
  pubkey: string
  'ca-fingerprint': string
  'system-start-time': string
  zram: boolean
  smtp: typeof customSmtp.validator._TYPE
  'password-hash': string
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
  'shutting-down': boolean
}

export interface PackageDataEntry {
  state: PackageState
  manifest: Manifest
  icon: string
  installed?: InstalledPackageInfo // when: installed
  actions?: Record<string, Action> // when: installed
  'install-progress'?: InstallProgress // when: installing, updating, restoring
}

// export type PackageDataEntry =
//   | PackageDataEntryInstalled
//   | PackageDataEntryNeedsUpdate
//   | PackageDataEntryRemoving
//   | PackageDataEntryRestoring
//   | PackageDataEntryUpdating
//   | PackageDataEntryInstalling

// export type PackageDataEntryBase = {
//   manifest: Manifest
//   icon: Url
// }

// export interface PackageDataEntryInstalled extends PackageDataEntryBase {
//   state: PackageState.Installed
//   installed: InstalledPackageInfo
//   actions: Record<string, Action>
// }

// export interface PackageDataEntryNeedsUpdate extends PackageDataEntryBase {
//   state: PackageState.NeedsUpdate
// }

// export interface PackageDataEntryRemoving extends PackageDataEntryBase {
//   state: PackageState.Removing
// }

// export interface PackageDataEntryRestoring extends PackageDataEntryBase {
//   state: PackageState.Restoring
//   'install-progress': InstallProgress
// }

// export interface PackageDataEntryUpdating extends PackageDataEntryBase {
//   state: PackageState.Updating
//   'install-progress': InstallProgress
// }

// export interface PackageDataEntryInstalling extends PackageDataEntryBase {
//   state: PackageState.Installing
//   'install-progress': InstallProgress
// }

export enum PackageState {
  Installing = 'installing',
  Installed = 'installed',
  Updating = 'updating',
  Removing = 'removing',
  Restoring = 'restoring',
  NeedsUpdate = 'needs-update',
}

export interface InstalledPackageInfo {
  status: Status
  'last-backup': string | null
  'installed-at': string
  'current-dependencies': Record<string, CurrentDependencyInfo>
  'dependency-info': Record<string, { title: string; icon: Url }>
  interfaceInfo: Record<string, InterfaceInfo>
  'marketplace-url': string | null
  'developer-key': string
  'has-config': boolean
  outboundProxy: ServiceOutboundProxy
}

export interface CurrentDependencyInfo {
  'health-checks': string[] // array of health check IDs
}

export interface InterfaceInfo {
  name: string
  description: string
  type: NetworkInterfaceType
  addressInfo: AddressInfo
}

export interface AddressInfo {
  ipInfo: IpInfo
  lanHostname: string
  torHostname: string
  domainInfo: DomainInfo | null
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
  'dependency-errors': { [id: string]: DependencyError | null }
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

export type DependencyError =
  | DependencyErrorNotInstalled
  | DependencyErrorNotRunning
  | DependencyErrorIncorrectVersion
  | DependencyErrorConfigUnsatisfied
  | DependencyErrorHealthChecksFailed
  | DependencyErrorTransitive

export enum DependencyErrorType {
  NotInstalled = 'not-installed',
  NotRunning = 'not-running',
  IncorrectVersion = 'incorrect-version',
  ConfigUnsatisfied = 'config-unsatisfied',
  HealthChecksFailed = 'health-checks-failed',
  Transitive = 'transitive',
}

export interface DependencyErrorNotInstalled {
  type: DependencyErrorType.NotInstalled
}

export interface DependencyErrorNotRunning {
  type: DependencyErrorType.NotRunning
}

export interface DependencyErrorIncorrectVersion {
  type: DependencyErrorType.IncorrectVersion
  expected: string // version range
  received: string // version
}

export interface DependencyErrorConfigUnsatisfied {
  type: DependencyErrorType.ConfigUnsatisfied
  error: string
}

export interface DependencyErrorHealthChecksFailed {
  type: DependencyErrorType.HealthChecksFailed
  check: HealthCheckResult
}

export interface DependencyErrorTransitive {
  type: DependencyErrorType.Transitive
}

export interface InstallProgress {
  readonly size: number | null
  readonly downloaded: number
  readonly 'download-complete': boolean
  readonly validated: number
  readonly 'validation-complete': boolean
  readonly unpacked: number
  readonly 'unpack-complete': boolean
}
