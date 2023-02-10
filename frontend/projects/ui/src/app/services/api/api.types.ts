import { Dump, Revision } from 'patch-db-client'
import { MarketplacePkg, StoreInfo, Manifest } from '@start9labs/marketplace'
import { PackagePropertiesVersioned } from 'src/app/util/properties.util'
import { InputSpec } from 'start-sdk/lib/config/configTypes'
import {
  DataModel,
  DependencyError,
} from 'src/app/services/patch-db/data-model'
import { EmbassyOSDiskInfo, LogsRes, ServerLogsReq } from '@start9labs/shared'

export module RR {
  // DB

  export type GetRevisionsRes = Revision[] | Dump<DataModel>

  export type GetDumpRes = Dump<DataModel>

  export type SetDBValueReq<T> = { pointer: string; value: T } // db.put.ui
  export type SetDBValueRes = null

  // auth

  export type LoginReq = {
    password: Encrypted | string
    metadata: SessionMetadata
  } // auth.login - unauthed
  export type loginRes = null

  export type LogoutReq = {} // auth.logout
  export type LogoutRes = null

  // server

  export type EchoReq = { message: string } // server.echo
  export type EchoRes = string

  export type GetSystemTimeReq = {} // server.time
  export type GetSystemTimeRes = string

  export type GetServerLogsReq = ServerLogsReq // server.logs & server.kernel-logs
  export type GetServerLogsRes = LogsRes

  export type FollowServerLogsReq = { limit?: number } // server.logs.follow & server.kernel-logs.follow
  export type FollowServerLogsRes = {
    'start-cursor': string
    guid: string
  }

  export type GetServerMetricsReq = {} // server.metrics
  export type GetServerMetricsRes = Metrics

  export type UpdateServerReq = { 'marketplace-url': string } // server.update
  export type UpdateServerRes = 'updating' | 'no-updates'

  export type RestartServerReq = {} // server.restart
  export type RestartServerRes = null

  export type ShutdownServerReq = {} // server.shutdown
  export type ShutdownServerRes = null

  export type SystemRebuildReq = {} // server.rebuild
  export type SystemRebuildRes = null

  // sessions

  export type GetSessionsReq = {} // sessions.list
  export type GetSessionsRes = {
    current: string
    sessions: { [hash: string]: Session }
  }

  export type KillSessionsReq = { ids: string[] } // sessions.kill
  export type KillSessionsRes = null

  // password

  export type UpdatePasswordReq = { password: string } // password.set
  export type UpdatePasswordRes = null

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

  export type GetWifiReq = {}
  export type GetWifiRes = {
    ssids: {
      [ssid: string]: number
    }
    connected: string | null
    country: string | null
    ethernet: boolean
    'available-wifi': AvailableWifi[]
  }

  export type AddWifiReq = {
    // wifi.add
    ssid: string
    password: string
    priority: number
    connect: boolean
  }
  export type AddWifiRes = null

  export type EnableWifiReq = { enable: boolean } // wifi.enable
  export type EnableWifiRes = null

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
  export type GetBackupTargetsRes = {
    'unknown-disks': UnknownDisk[]
    saved: BackupTarget[]
  }

  export type AddCifsBackupTargetReq = {
    name: string
    path: string
    hostname: string
    username: string
    password?: string
  } // backup.target.cifs.add
  export type AddCloudBackupTargetReq = {
    name: string
    path: string
    provider: CloudProvider
    [params: string]: any
  } // backup.target.cloud.add
  export type AddDiskBackupTargetReq = {
    logicalname: string
    name: string
    path: string
  } // backup.target.disk.add
  export type AddBackupTargetRes = BackupTarget

  export type UpdateCifsBackupTargetReq = AddCifsBackupTargetReq & {
    id: string
  } // backup.target.cifs.update
  export type UpdateCloudBackupTargetReq = AddCloudBackupTargetReq & {
    id: string
  } // backup.target.cloud.update
  export type UpdateDiskBackupTargetReq = Omit<
    AddDiskBackupTargetReq,
    'logicalname'
  > & {
    id: string
  } // backup.target.disk.update
  export type UpdateBackupTargetRes = AddBackupTargetRes

  export type RemoveBackupTargetReq = { id: string } // backup.target.remove
  export type RemoveBackupTargetRes = null

  export type GetBackupJobsReq = {} // backup.job.list
  export type GetBackupJobsRes = BackupJob[]

  export type CreateBackupJobReq = {
    name: string
    'target-id': string
    cron: string
    'package-ids': string[]
    now: boolean
  } // backup.job.create
  export type CreateBackupJobRes = BackupJob

  export type UpdateBackupJobReq = Omit<CreateBackupJobReq, 'now'> & {
    id: string
  } // backup.job.update
  export type UpdateBackupJobRes = CreateBackupJobRes

  export type DeleteBackupJobReq = { id: string } // backup.job.delete
  export type DeleteBackupJobRes = null

  export type GetBackupRunsReq = {} // backup.runs
  export type GetBackupRunsRes = BackupRun[]

  export type GetBackupInfoReq = { 'target-id': string; password: string } // backup.target.info
  export type GetBackupInfoRes = BackupInfo

  export type CreateBackupReq = { 'target-id': string; 'package-ids': string[] } // backup.create
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
    'version-spec'?: string
    'version-priority'?: 'min' | 'max'
    'marketplace-url': string
  } // package.install
  export type InstallPackageRes = null

  export type DryUpdatePackageReq = { id: string; version: string } // package.update.dry
  export type DryUpdatePackageRes = Breakages

  export type GetPackageConfigReq = { id: string } // package.config.get
  export type GetPackageConfigRes = { spec: InputSpec; config: object }

  export type DrySetPackageConfigReq = { id: string; config: object } // package.config.set.dry
  export type DrySetPackageConfigRes = Breakages

  export type SetPackageConfigReq = DrySetPackageConfigReq // package.config.set
  export type SetPackageConfigRes = null

  export type RestorePackagesReq = {
    // package.backup.restore
    ids: string[]
    'target-id': string
    password: string
  }
  export type RestorePackagesRes = null

  export type ExecutePackageActionReq = {
    id: string
    'action-id': string
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
    'dependency-id': string
    'dependent-id': string
  } // package.dependency.configure.dry
  export type DryConfigureDependencyRes = {
    'old-config': object
    'new-config': object
    spec: InputSpec
  }

  export type SideloadPackageReq = {
    manifest: Manifest
    icon: string // base64
    size: number // bytes
  }
  export type SideloadPacakgeRes = string //guid

  // marketplace

  export type EnvInfo = {
    'server-id': string
    'eos-version': string
  }
  export type GetMarketplaceInfoReq = EnvInfo
  export type GetMarketplaceInfoRes = StoreInfo

  export type GetMarketplaceEosReq = EnvInfo
  export type GetMarketplaceEosRes = MarketplaceEOS

  export type GetMarketplacePackagesReq = {
    ids?: { id: string; version: string }[]
    'eos-version-compat': string
    // iff !ids
    category?: string
    query?: string
    page?: number
    'per-page'?: number
  }
  export type GetMarketplacePackagesRes = MarketplacePkg[]

  export type GetReleaseNotesReq = { id: string }
  export type GetReleaseNotesRes = { [version: string]: string }
}

export interface MarketplaceEOS {
  version: string
  headline: string
  'release-notes': { [version: string]: string }
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

export interface Metrics {
  [key: string]: {
    [key: string]: {
      value: string | number | null
      unit?: string
    }
  }
}

export interface Metric {
  [key: string]: {
    value: string | number | null
    unit?: string
  }
}

export interface Session {
  'last-active': string
  'user-agent': string
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

export type RemoteBackupTarget = CifsBackupTarget | CloudBackupTarget
export type BackupTarget = RemoteBackupTarget | DiskBackupTarget

export type BackupTargetType = 'disk' | 'cifs' | 'cloud'

export interface UnknownDisk {
  logicalname: string
  vendor: string | null
  model: string | null
  label: string | null
  capacity: number
  used: number | null
}

export interface BaseBackupTarget {
  id: string
  type: BackupTargetType
  name: string
  mountable: boolean
  path: string
  'embassy-os': EmbassyOSDiskInfo | null
}

export interface DiskBackupTarget extends UnknownDisk, BaseBackupTarget {
  type: 'disk'
}

export interface CifsBackupTarget extends BaseBackupTarget {
  type: 'cifs'
  hostname: string
  username: string
}

export interface CloudBackupTarget extends BaseBackupTarget {
  type: 'cloud'
  provider: 'dropbox' | 'google-drive'
}

export interface BackupRun {
  id: string
  'started-at': string
  'completed-at': string
  'package-ids': string[]
  job: BackupJob
  report: BackupReport
}

export interface BackupJob {
  id: string
  name: string
  target: BackupTarget
  cron: string // '* * * * * *' https://cloud.google.com/scheduler/docs/configuring/cron-job-schedules
  'package-ids': string[]
}

export interface BackupInfo {
  version: string
  timestamp: string
  'package-backups': {
    [id: string]: PackageBackupInfo
  }
}

export interface PackageBackupInfo {
  title: string
  version: string
  'os-version': string
  timestamp: string
}

export interface ServerSpecs {
  [key: string]: string | number
}

export interface SSHKey {
  'created-at': string
  alg: string
  hostname: string
  fingerprint: string
}

export type ServerNotifications = ServerNotification<any>[]

export interface ServerNotification<T extends number> {
  id: number
  'package-id': string | null
  'created-at': string
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

export type CloudProvider = 'dropbox' | 'google-drive'
