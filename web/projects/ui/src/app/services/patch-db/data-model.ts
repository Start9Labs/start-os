import { BackupJob, ServerNotifications } from '../api/api.types'
import { Url } from '@start9labs/shared'
import { Manifest } from '@start9labs/marketplace'
import { T } from '@start9labs/start-sdk'
import {
  ActionMetadata,
  HostnameInfo,
} from '@start9labs/start-sdk/cjs/sdk/lib/types'
import { customSmtp } from '@start9labs/start-sdk/cjs/sdk/lib/config/configConstants'

export interface DataModel {
  serverInfo: ServerInfo
  packageData: { [id: string]: PackageDataEntry }
  ui: UIData
}

export interface UIData {
  name: string | null
  ackWelcome: string // emver
  marketplace: UIMarketplaceData
  gaming: {
    snake: {
      highScore: number
    }
  }
  ackInstructions: Record<string, boolean>
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
  selectedUrl: string
  knownHosts: {
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
  lastBackup: string | null
  unreadNotifications: {
    count: number
    recent: ServerNotifications
  }
  statusInfo: ServerStatusInfo
  eosVersionCompat: string
  pubkey: string
  caFingerprint: string
  ntpSynced: boolean
  smtp: typeof customSmtp.validator._TYPE
  passwordHash: string
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
  outboundProxy: string | null
}

export type DomainInfo = {
  domain: string
  subdomain: string | null
}

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
  | { proxy: string }
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
}

export interface IpInfo {
  [iface: string]: {
    wireless: boolean
    ipv4: string | null
    ipv6: string | null
  }
}

export interface ServerStatusInfo {
  currentBackup: null | {
    job: BackupJob
    backupProgress: Record<string, boolean>
  }
  updated: boolean
  updateProgress: { size: number | null; downloaded: number } | null
  restarting: boolean
  shuttingDown: boolean
}

export type PackageDataEntry<T extends StateInfo = StateInfo> = {
  stateInfo: T
  icon: Url
  status: Status
  actions: Record<string, ActionMetadata>
  lastBackup: string | null
  currentDependencies: { [id: string]: CurrentDependencyInfo }
  dependencyInfo: {
    [id: string]: {
      title: string
      icon: Url
    }
  }
  serviceInterfaces: Record<string, T.ServiceInterfaceWithHostInfo>
  marketplaceUrl: string | null
  developerKey: string
  installedAt: string
  outboundProxy: string | null
}

export type StateInfo = InstalledState | InstallingState | UpdatingState

export type InstalledState = {
  state: PackageState.Installed | PackageState.Removing
  manifest: Manifest
  installingInfo?: undefined
}

export type InstallingState = {
  state: PackageState.Installing | PackageState.Restoring
  installingInfo: InstallingInfo
  manifest?: undefined
}

export type UpdatingState = {
  state: PackageState.Updating
  installingInfo: InstallingInfo
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
  versionRange: string
  healthChecks: string[] // array of health check IDs
}

export interface Status {
  configured: boolean
  main: MainStatus
  dependencyConfigErrors: { [id: string]: string | null }
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
  health: Record<string, HealthCheckResult>
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
  message: string
}

export type InstallingInfo = {
  progress: FullProgress
  newManifest: Manifest
}

export type FullProgress = {
  overall: Progress
  phases: { name: string; progress: Progress }[]
}
export type Progress = boolean | { done: number; total: number | null } // false means indeterminate. true means complete
