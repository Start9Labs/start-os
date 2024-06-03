import { Dump, Revision } from 'patch-db-client'
import { MarketplacePkg, StoreInfo } from '@start9labs/marketplace'
import { PackagePropertiesVersioned } from 'src/app/util/properties.util'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StartOSDiskInfo, LogsRes, ServerLogsReq } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

export module RR {
  // DB

  export type GetRevisionsRes = Revision[] | Dump<DataModel>

  export type GetDumpRes = Dump<DataModel>

  export type SetDBValueReq<T> = { pointer: string; value: T } // db.put.ui
  export type SetDBValueRes = null

  // auth

  export type LoginReq = {
    password: string
    metadata: SessionMetadata
  } // auth.login - unauthed
  export type loginRes = null

  export type LogoutReq = {} // auth.logout
  export type LogoutRes = null

  export type ResetPasswordReq = {
    oldPassword: string
    newPassword: string
  } // auth.reset-password
  export type ResetPasswordRes = null

  // diagnostic

  export type DiagnosticErrorRes = {
    code: number
    message: string
    data: { details: string }
  }

  // server

  export type EchoReq = { message: string; timeout?: number } // server.echo
  export type EchoRes = string

  export type GetSystemTimeReq = {} // server.time
  export type GetSystemTimeRes = {
    now: string
    uptime: number // seconds
  }

  export type GetServerLogsReq = ServerLogsReq // server.logs & server.kernel-logs
  export type GetServerLogsRes = LogsRes

  export type FollowServerLogsReq = { limit?: number } // server.logs.follow & server.kernel-logs.follow
  export type FollowServerLogsRes = {
    startCursor: string
    guid: string
  }

  export type GetServerMetricsReq = {} // server.metrics
  export type GetServerMetricsRes = Metrics

  export type UpdateServerReq = { registry: string } // server.update
  export type UpdateServerRes = 'updating' | 'no-updates'

  export type RestartServerReq = {} // server.restart
  export type RestartServerRes = null

  export type ShutdownServerReq = {} // server.shutdown
  export type ShutdownServerRes = null

  export type DiskRepairReq = {} // server.disk.repair
  export type DiskRepairRes = null

  export type ResetTorReq = {
    wipeState: boolean
    reason: string
  } // net.tor.reset
  export type ResetTorRes = null

  // sessions

  export type GetSessionsReq = {} // sessions.list
  export type GetSessionsRes = {
    current: string
    sessions: { [hash: string]: Session }
  }

  export type KillSessionsReq = { ids: string[] } // sessions.kill
  export type KillSessionsRes = null

  // notification

  export type GetNotificationsReq = {
    before?: number
    limit?: number
  } // notification.list
  export type GetNotificationsRes = ServerNotification<number>[]

  export type DeleteNotificationReq = { id: number } // notification.delete
  export type DeleteNotificationRes = null

  export type DeleteAllNotificationsReq = { before: number } // notification.delete-before
  export type DeleteAllNotificationsRes = null

  // wifi

  export type SetWifiCountryReq = { country: string }
  export type SetWifiCountryRes = null

  export type GetWifiReq = {}
  export type GetWifiRes = {
    ssids: {
      [ssid: string]: number
    }
    connected: string | null
    country: string | null
    ethernet: boolean
    availableWifi: AvailableWifi[]
  }

  export type AddWifiReq = {
    // wifi.add
    ssid: string
    password: string
    priority: number
    connect: boolean
  }
  export type AddWifiRes = null

  export type ConnectWifiReq = { ssid: string } // wifi.connect
  export type ConnectWifiRes = null

  export type DeleteWifiReq = { ssid: string } // wifi.delete
  export type DeleteWifiRes = null

  // ssh

  export type GetSSHKeysReq = {} // ssh.list
  export type GetSSHKeysRes = SSHKey[]

  export type AddSSHKeyReq = { key: string } // ssh.add
  export type AddSSHKeyRes = SSHKey

  export type DeleteSSHKeyReq = { fingerprint: string } // ssh.delete
  export type DeleteSSHKeyRes = null

  // backup

  export type GetBackupTargetsReq = {} // backup.target.list
  export type GetBackupTargetsRes = { [id: string]: BackupTarget }

  export type AddBackupTargetReq = {
    // backup.target.cifs.add
    hostname: string
    path: string
    username: string
    password: string | null
  }
  export type AddBackupTargetRes = { [id: string]: CifsBackupTarget }

  export type UpdateBackupTargetReq = AddBackupTargetReq & { id: string } // backup.target.cifs.update
  export type UpdateBackupTargetRes = AddBackupTargetRes

  export type RemoveBackupTargetReq = { id: string } // backup.target.cifs.remove
  export type RemoveBackupTargetRes = null

  export type GetBackupInfoReq = { targetId: string; password: string } // backup.target.info
  export type GetBackupInfoRes = BackupInfo

  export type CreateBackupReq = {
    // backup.create
    targetId: string
    packageIds: string[]
    oldPassword: string | null
    password: string
  }
  export type CreateBackupRes = null

  // package

  export type GetPackagePropertiesReq = { id: string } // package.properties
  export type GetPackagePropertiesRes<T extends number> =
    PackagePropertiesVersioned<T>

  export type GetPackageLogsReq = ServerLogsReq & { id: string } // package.logs
  export type GetPackageLogsRes = LogsRes

  export type FollowPackageLogsReq = FollowServerLogsReq & { id: string } // package.logs.follow
  export type FollowPackageLogsRes = FollowServerLogsRes

  export type GetPackageMetricsReq = { id: string } // package.metrics
  export type GetPackageMetricsRes = Metric

  export type InstallPackageReq = {
    id: string
    versionSpec?: string
    versionPriority?: 'min' | 'max'
    registry: string
  } // package.install
  export type InstallPackageRes = null

  export type GetPackageConfigReq = { id: string } // package.config.get
  export type GetPackageConfigRes = { spec: ConfigSpec; config: object }

  export type DrySetPackageConfigReq = { id: string; config: object } // package.config.set.dry
  export type DrySetPackageConfigRes = Breakages

  export type SetPackageConfigReq = DrySetPackageConfigReq // package.config.set
  export type SetPackageConfigRes = null

  export type RestorePackagesReq = {
    // package.backup.restore
    ids: string[]
    targetId: string
    oldPassword: string | null
    password: string
  }
  export type RestorePackagesRes = null

  export type ExecutePackageActionReq = {
    id: string
    actionId: string
    input?: object
  } // package.action
  export type ExecutePackageActionRes = ActionResponse

  export type StartPackageReq = { id: string } // package.start
  export type StartPackageRes = null

  export type RestartPackageReq = { id: string } // package.restart
  export type RestartPackageRes = null

  export type StopPackageReq = { id: string } // package.stop
  export type StopPackageRes = null

  export type UninstallPackageReq = { id: string } // package.uninstall
  export type UninstallPackageRes = null

  export type DryConfigureDependencyReq = {
    dependencyId: string
    dependentId: string
  } // package.dependency.configure.dry
  export type DryConfigureDependencyRes = {
    oldConfig: object
    newConfig: object
    spec: ConfigSpec
  }

  export type SideloadPackageReq = {
    manifest: T.Manifest
    icon: string // base64
  }
  export type SideloadPacakgeRes = string //guid

  // marketplace

  export type GetMarketplaceInfoReq = { serverId: string }
  export type GetMarketplaceInfoRes = StoreInfo

  export type GetMarketplaceEosReq = { serverId: string }
  export type GetMarketplaceEosRes = MarketplaceEOS

  export type GetMarketplacePackagesReq = {
    ids?: { id: string; version: string }[]
    // iff !ids
    category?: string
    query?: string
    page?: number
    perPage?: number
  }
  export type GetMarketplacePackagesRes = MarketplacePkg[]

  export type GetReleaseNotesReq = { id: string }
  export type GetReleaseNotesRes = { [version: string]: string }
}

export interface MarketplaceEOS {
  version: string
  headline: string
  releaseNotes: { [version: string]: string }
}

export interface Breakages {
  [id: string]: TaggedDependencyError
}

export interface TaggedDependencyError {
  dependency: string
  error: DependencyError
}

export interface ActionResponse {
  message: string
  value: string | null
  copyable: boolean
  qr: boolean
}

interface MetricData {
  value: string
  unit: string
}

export interface Metrics {
  general: {
    temperature: MetricData | null
  }
  memory: {
    total: MetricData
    percentageUsed: MetricData
    used: MetricData
    available: MetricData
    zramTotal: MetricData
    zramUsed: MetricData
    zramAvailable: MetricData
  }
  cpu: {
    percentageUsed: MetricData
    idle: MetricData
    userSpace: MetricData
    kernelSpace: MetricData
    wait: MetricData
  }
  disk: {
    capacity: MetricData
    percentageUsed: MetricData
    used: MetricData
    available: MetricData
  }
}

export interface Metric {
  [key: string]: {
    value: string | number | null
    unit?: string
  }
}

export interface Session {
  lastActive: string
  userAgent: string
  metadata: SessionMetadata
}

export interface SessionMetadata {
  platforms: PlatformType[]
}

export type PlatformType =
  | 'cli'
  | 'ios'
  | 'ipad'
  | 'iphone'
  | 'android'
  | 'phablet'
  | 'tablet'
  | 'cordova'
  | 'capacitor'
  | 'electron'
  | 'pwa'
  | 'mobile'
  | 'mobileweb'
  | 'desktop'
  | 'hybrid'

export type BackupTarget = DiskBackupTarget | CifsBackupTarget

export interface DiskBackupTarget {
  type: 'disk'
  vendor: string | null
  model: string | null
  logicalname: string | null
  label: string | null
  capacity: number
  used: number | null
  startOs: StartOSDiskInfo | null
}

export interface CifsBackupTarget {
  type: 'cifs'
  hostname: string
  path: string
  username: string
  mountable: boolean
  startOs: StartOSDiskInfo | null
}

export type RecoverySource = DiskRecoverySource | CifsRecoverySource

export interface DiskRecoverySource {
  type: 'disk'
  logicalname: string // partition logicalname
}

export interface CifsRecoverySource {
  type: 'cifs'
  hostname: string
  path: string
  username: string
  password: string
}

export interface BackupInfo {
  version: string
  timestamp: string
  packageBackups: {
    [id: string]: PackageBackupInfo
  }
}

export interface PackageBackupInfo {
  title: string
  version: string
  osVersion: string
  timestamp: string
}

export interface ServerSpecs {
  [key: string]: string | number
}

export interface SSHKey {
  createdAt: string
  alg: string
  hostname: string
  fingerprint: string
}

export type ServerNotifications = ServerNotification<any>[]

export interface ServerNotification<T extends number> {
  id: number
  packageId: string | null
  createdAt: string
  code: T
  level: NotificationLevel
  title: string
  message: string
  data: NotificationData<T>
}

export enum NotificationLevel {
  Success = 'success',
  Info = 'info',
  Warning = 'warning',
  Error = 'error',
}

export type NotificationData<T> = T extends 0
  ? null
  : T extends 1
  ? BackupReport
  : any

export interface BackupReport {
  server: {
    attempted: boolean
    error: string | null
  }
  packages: {
    [id: string]: {
      error: string | null
    }
  }
}

export interface AvailableWifi {
  ssid: string
  strength: number
  security: string[]
}

declare global {
  type Stringified<T> = string & {
    [P in keyof T]: T[P]
  }

  interface JSON {
    stringify<T>(
      value: T,
      replacer?: (key: string, value: any) => any,
      space?: string | number,
    ): string & Stringified<T>
    parse<T>(text: Stringified<T>, reviver?: (key: any, value: any) => any): T
  }
}

export type Encrypted = {
  encrypted: string
}

export type DependencyError =
  | DependencyErrorNotInstalled
  | DependencyErrorNotRunning
  | DependencyErrorIncorrectVersion
  | DependencyErrorConfigUnsatisfied
  | DependencyErrorHealthChecksFailed
  | DependencyErrorTransitive

export interface DependencyErrorNotInstalled {
  type: 'notInstalled'
}

export interface DependencyErrorNotRunning {
  type: 'notRunning'
}

export interface DependencyErrorIncorrectVersion {
  type: 'incorrectVersion'
  expected: string // version range
  received: string // version
}

export interface DependencyErrorConfigUnsatisfied {
  type: 'configUnsatisfied'
}

export interface DependencyErrorHealthChecksFailed {
  type: 'healthChecksFailed'
  check: T.HealthCheckResult
}

export interface DependencyErrorTransitive {
  type: 'transitive'
}
