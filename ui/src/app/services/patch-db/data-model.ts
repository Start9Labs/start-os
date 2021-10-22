import { ConfigSpec } from 'src/app/pkg-config/config-types'

export interface DataModel {
  'server-info': ServerInfo
  'package-data': { [id: string]: PackageDataEntry }
  'recovered-packages': { [id: string]: RecoveredPackageDataEntry }
  ui: UIData
}

export interface UIData {
  name: string
  'auto-check-updates': boolean
  'pkg-order': string[]
  'ack-welcome': string // EOS version
  'ack-share-stats': boolean
}

export interface ServerInfo {
  id: string
  version: string
  'last-backup': string | null
  'lan-address': URL
  'tor-address': URL
  status: ServerStatus
  'eos-marketplace': URL
  'package-marketplace': URL | null // uses EOS marketplace if null
  'share-stats': boolean
  'unread-notification-count': number
  'update-progress'?: {
    size: number
    downloaded: number
  }
  'eos-version-compat': string
}

export enum ServerStatus {
  Running = 'running',
  Updating = 'updating',
  Updated = 'updated',
  BackingUp = 'backing-up',
}
export interface RecoveredPackageDataEntry {
  title: string,
  icon: URL,
  version: string,
}

export interface PackageDataEntry {
  state: PackageState
  'static-files': {
    license: URL
    instructions: URL
    icon: URL
  }
  manifest: Manifest
  installed?: InstalledPackageDataEntry, // exists when: installed, updating
  'install-progress'?: InstallProgress, // exists when: installing, updating
}

export interface InstallProgress {
  size: number | null
  downloaded: number
  'download-complete': boolean
  validated: number
  'validation-complete': boolean
  unpacked: number
  'unpack-complete': boolean
}

export interface InstalledPackageDataEntry {
  status: Status
  manifest: Manifest,
  'last-backup': string | null
  'system-pointers': any[]
  'current-dependents': { [id: string]: CurrentDependencyInfo }
  'current-dependencies': { [id: string]: CurrentDependencyInfo }
  'dependency-info': {
    [id: string]: {
      manifest: Manifest
      icon: URL
    }
  }
  'interface-addresses': {
    [id: string]: { 'tor-address': string, 'lan-address': string }
  }
}

export interface CurrentDependencyInfo {
  pointers: any[]
  'health-checks': string[] // array of health check IDs
}

export enum PackageState {
  Installing = 'installing',
  Installed = 'installed',
  Updating = 'updating',
  Removing = 'removing',
}

export interface Manifest {
  id: string
  title: string
  version: string
  description: {
    short: string
    long: string
  }
  'release-notes': string
  license: string // name
  'wrapper-repo': URL
  'upstream-repo': URL
  'support-site': URL
  'marketing-site': URL
  'donation-url': URL | null
  alerts: {
    install: string | null
    uninstall: string | null
    restore: string | null
    start: string | null
    stop: string | null
  }
  main: ActionImpl
  'health-checks': { [id: string]: ActionImpl & { critical: boolean } }
  config: ConfigActions | null
  volumes: { [id: string]: Volume }
  'min-os-version': string
  interfaces: { [id: string]: InterfaceDef }
  backup: BackupActions
  migrations: Migrations
  actions: { [id: string]: Action }
  permissions: any // @TODO
  dependencies: DependencyInfo
}

export interface ActionImpl {
  type: 'docker'
  image: string
  system: boolean
  entrypoint: string
  args: string[]
  mounts: { [id: string]: string }
  'io-format': DockerIoFormat | null
  inject: boolean
  'shm-size': string
}

export enum DockerIoFormat {
  Json = 'json',
  Yaml = 'yaml',
  Cbor = 'cbor',
  Toml = 'toml',
}

export interface ConfigActions {
  get: ActionImpl
  set: ActionImpl
}

export type Volume = VolumeData

export interface VolumeData {
  type: VolumeType.Data
  readonly: boolean
}

export interface VolumeAssets {
  type: VolumeType.Assets
}

export interface VolumePointer {
  type: VolumeType.Pointer
  'package-id': string
  'volume-id': string
  path: string
  readonly: boolean
}

export interface VolumeCertificate {
  type: VolumeType.Certificate
  'interface-id': string
}

export interface VolumeBackup {
  type: VolumeType.Backup
  readonly: boolean
}

export enum VolumeType {
  Data = 'data',
  Assets = 'assets',
  Pointer = 'pointer',
  Certificate = 'certificate',
  Backup = 'backup',
}

export interface InterfaceDef {
  name: string
  description: string
  'tor-config': TorConfig | null
  'lan-config': LanConfig | null
  ui: boolean
  protocols: string[]
}

export interface TorConfig {
  'port-mapping': { [port: number]: number }
}

export type LanConfig = {
  [port: number]: { ssl: boolean, mapping: number }
}

export interface BackupActions {
  create: ActionImpl
  restore: ActionImpl
}

export interface Migrations {
  from: { [versionRange: string]: ActionImpl }
  to: { [versionRange: string]: ActionImpl }
}

export interface Action {
  name: string
  description: string
  warning: string | null
  implementation: ActionImpl
  'allowed-statuses': (PackageMainStatus.Stopped | PackageMainStatus.Running)[]
  'input-spec': ConfigSpec
}

export interface Status {
  configured: boolean
  main: MainStatus
  'dependency-errors': { [id: string]: DependencyError | null }
}

export type MainStatus = MainStatusStopped | MainStatusStopping | MainStatusRunning | MainStatusBackingUp | MainStatusRestoring

export interface MainStatusStopped {
  status: PackageMainStatus.Stopped
}

export interface MainStatusStopping {
  status: PackageMainStatus.Stopping
}

export interface MainStatusRunning {
  status: PackageMainStatus.Running
  started: string // UTC date string
  health: { [id: string]: HealthCheckResult }
}

export interface MainStatusBackingUp {
  status: PackageMainStatus.BackingUp
  started: string | null // UTC date string
}

export interface MainStatusRestoring {
  status: PackageMainStatus.Restoring
  running: boolean
}

export enum PackageMainStatus {
  Running = 'running',
  Stopping = 'stopping',
  Stopped = 'stopped',
  BackingUp = 'backing-up',
  Restoring = 'restoring',
}

export type HealthCheckResult = HealthCheckResultStarting |
                                HealthCheckResultLoading |
                                HealthCheckResultDisabled |
                                HealthCheckResultSuccess |
                                HealthCheckResultFailure

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
}

export interface HealthCheckResultLoading {
  result: HealthResult.Loading
  message: string
}

export interface HealthCheckResultFailure {
  result: HealthResult.Failure
  error: string
}

export type DependencyError = DependencyErrorNotInstalled |
                              DependencyErrorNotRunning |
                              DependencyErrorIncorrectVersion |
                              DependencyErrorConfigUnsatisfied |
                              DependencyErrorHealthChecksFailed |
                              DependencyErrorInterfaceHealthChecksFailed |
                              DependencyErrorTransitive

export enum DependencyErrorType {
  NotInstalled = 'not-installed',
  NotRunning = 'not-running',
  IncorrectVersion = 'incorrect-version',
  ConfigUnsatisfied = 'config-unsatisfied',
  HealthChecksFailed = 'health-checks-failed',
  InterfaceHealthChecksFailed = 'interface-health-checks-failed',
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

export interface DependencyErrorInterfaceHealthChecksFailed {
  type: DependencyErrorType.InterfaceHealthChecksFailed
  failures: { [id: string]: HealthCheckResult }
}

export interface DependencyErrorTransitive {
  type: DependencyErrorType.Transitive
}

export interface DependencyInfo {
  [id: string]: DependencyEntry
}

export interface DependencyEntry {
  version: string
  requirement: {
    type: 'opt-in'
    how: string
  } | {
    type: 'opt-out'
    how: string
  } | {
    type: 'required'
  }
  description: string | null
  critical: boolean,
  config: {
    check: ActionImpl,
    'auto-configure': ActionImpl
  }
}

export type URL = string
