import { Url } from '@start9labs/shared'
import { Manifest } from '@start9labs/marketplace'
import { BasicInfo } from 'src/app/pages/developer-routes/developer-menu/form-info'
import { types } from '@start9labs/start-sdk'
import { InputSpec } from '@start9labs/start-sdk/cjs/sdk/lib/config/configTypes'
import { ActionMetadata } from '@start9labs/start-sdk/cjs/sdk/lib/types'
type ServiceInterfaceWithHostInfo = types.ServiceInterfaceWithHostInfo

export interface DataModel {
  'server-info': ServerInfo
  'package-data': { [id: string]: PackageDataEntry }
  ui: UIData
}

export interface UIData {
  name: string | null
  'ack-welcome': string // eOS emver
  marketplace: UIMarketplaceData
  dev: DevData
  gaming: {
    snake: {
      'high-score': number
    }
  }
  'ack-instructions': Record<string, boolean>
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

export interface DevData {
  [id: string]: DevProjectData
}

export interface DevProjectData {
  name: string
  instructions: string
  config: string
  'basic-info'?: BasicInfo
}

export interface ServerInfo {
  id: string
  version: string
  'last-backup': string | null
  'lan-address': Url
  'tor-address': Url
  'ip-info': IpInfo
  'last-wifi-region': string | null
  'unread-notification-count': number
  'status-info': ServerStatusInfo
  'eos-version-compat': string
  'password-hash': string
  hostname: string
  pubkey: string
  'ca-fingerprint': string
  'ntp-synced': boolean
  platform: string
}

export interface IpInfo {
  [iface: string]: {
    ipv4: string | null
    ipv6: string | null
  }
}

export interface ServerStatusInfo {
  'backup-progress': null | {
    [packageId: string]: {
      complete: boolean
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
  actions: Record<string, ActionMetadata>
  'last-backup': string | null
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
  versionRange: string
  'health-checks': string[] // array of health check IDs
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
  'new-manifest': Manifest
}

export type FullProgress = {
  overall: Progress
  phases: { name: string; progress: Progress }[]
}
export type Progress = boolean | { done: number; total: number | null } // false means indeterminate. true means complete
