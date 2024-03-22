import { Url } from '@start9labs/shared'
import { Manifest } from '@start9labs/marketplace'
import { types } from '@start9labs/start-sdk'
import { ActionMetadata } from '@start9labs/start-sdk/cjs/sdk/lib/types'
type ServiceInterfaceWithHostInfo = types.ServiceInterfaceWithHostInfo

export interface DataModel {
  serverInfo: ServerInfo
  packageData: { [id: string]: PackageDataEntry }
  ui: UIData
}

export interface UIData {
  name: string | null
  ackWelcome: string // eOS emver
  marketplace: UIMarketplaceData
  gaming: {
    snake: {
      highScore: number
    }
  }
  ackInstructions: Record<string, boolean>
  theme: string
  widgets: readonly Widget[]
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
  lastBackup: string | null
  lanAddress: Url
  torAddress: Url
  ipInfo: IpInfo
  lastWifiRegion: string | null
  unreadNotificationCount: number
  statusInfo: ServerStatusInfo
  eosVersionCompat: string
  passwordHash: string
  hostname: string
  pubkey: string
  caFingerprint: string
  ntpSynced: boolean
  platform: string
}

export interface IpInfo {
  [iface: string]: {
    ipv4: string | null
    ipv6: string | null
  }
}

export interface ServerStatusInfo {
  backupProgress: null | {
    [packageId: string]: {
      complete: boolean
    }
  }
  updated: boolean
  updateProgress: { size: number | null; downloaded: number } | null
  restarting: boolean
  shuttingDown: boolean
}

export enum ServerStatus {
  Running = 'running',
  Updated = 'updated',
  BackingUp = 'backing-up',
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
  serviceInterfaces: Record<string, ServiceInterfaceWithHostInfo>
  marketplaceUrl: string | null
  developerKey: string
}

export type StateInfo = InstalledState | InstallingState | UpdatingState

export type InstalledState = {
  state: PackageState.Installed | PackageState.Removing
  manifest: Manifest
}

export type InstallingState = {
  state: PackageState.Installing | PackageState.Restoring
  installingInfo: InstallingInfo
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

export interface MainStatusStopped {
  status: PackageMainStatus.Stopped
}

export interface MainStatusStopping {
  status: PackageMainStatus.Stopping
  timeout: string
}

export interface MainStatusStarting {
  status: PackageMainStatus.Starting
  restarting: boolean
}

export interface MainStatusRunning {
  status: PackageMainStatus.Running
  started: string // UTC date string
  health: Record<string, HealthCheckResult>
}

export interface MainStatusBackingUp {
  status: PackageMainStatus.BackingUp
  started: string | null // UTC date string
}

export interface MainStatusRestarting {
  status: PackageMainStatus.Restarting
}

export enum PackageMainStatus {
  Starting = 'starting',
  Running = 'running',
  Stopping = 'stopping',
  Stopped = 'stopped',
  BackingUp = 'backing-up',
  Restarting = 'restarting',
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
