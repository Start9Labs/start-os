import { Dump, Revision } from 'patch-db-client'
import { PackagePropertiesVersioned } from 'src/app/util/properties.util'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import {
  DataModel,
  DependencyError,
  Manifest,
  URL,
} from 'src/app/services/patch-db/data-model'

export module RR {
  // DB

  export type GetRevisionsRes = Revision[] | Dump<DataModel>

  export type GetDumpRes = Dump<DataModel>

  export type SetDBValueReq = WithExpire<{ pointer: string; value: any }> // db.put.ui
  export type SetDBValueRes = WithRevision<null>

  // auth

  export type LoginReq = { password: string; metadata: SessionMetadata } // auth.login - unauthed
  export type loginRes = null

  export type LogoutReq = {} // auth.logout
  export type LogoutRes = null

  // server

  export type SetShareStatsReq = WithExpire<{ value: boolean }> // server.config.share-stats
  export type SetShareStatsRes = WithRevision<null>

  export type GetServerLogsReq = {
    cursor?: string
    before_flag?: boolean
    limit?: number
  }
  export type GetServerLogsRes = LogsRes

  export type GetServerMetricsReq = {} // server.metrics
  export type GetServerMetricsRes = Metrics

  export type UpdateServerReq = WithExpire<{}> // server.update
  export type UpdateServerRes = WithRevision<'updating' | 'no-updates'>

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

  export type KillSessionsReq = WithExpire<{ ids: string[] }> // sessions.kill
  export type KillSessionsRes = WithRevision<null>

  // password

  export type UpdatePasswordReq = { password: string } // password.set
  export type UpdatePasswordRes = null

  // notification

  export type GetNotificationsReq = WithExpire<{
    before?: number
    limit?: number
  }> // notification.list
  export type GetNotificationsRes = WithRevision<ServerNotification<number>[]>

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
    connected?: string
    country: string
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

  export type GetBackupInfoReq = { 'target-id': string; password: string } // backup.target.info
  export type GetBackupInfoRes = BackupInfo

  export type CreateBackupReq = WithExpire<{
    // backup.create
    'target-id': string
    'old-password': string | null
    password: string
  }>
  export type CreateBackupRes = WithRevision<null>

  // package

  export type GetPackagePropertiesReq = { id: string } // package.properties
  export type GetPackagePropertiesRes<T extends number> =
    PackagePropertiesVersioned<T>

  export type LogsRes = {
    entries: Log[]
    'start-cursor'?: string
    'end-cursor'?: string
  }

  export type GetPackageLogsReq = {
    id: string
    cursor?: string
    before_flag?: boolean
    limit?: number
  } // package.logs
  export type GetPackageLogsRes = LogsRes

  export type GetPackageMetricsReq = { id: string } // package.metrics
  export type GetPackageMetricsRes = Metric

  export type InstallPackageReq = WithExpire<{
    id: string
    'version-spec'?: string
  }> // package.install
  export type InstallPackageRes = WithRevision<null>

  export type DryUpdatePackageReq = { id: string; version: string } // package.update.dry
  export type DryUpdatePackageRes = Breakages

  export type GetPackageConfigReq = { id: string } // package.config.get
  export type GetPackageConfigRes = { spec: ConfigSpec; config: object }

  export type DrySetPackageConfigReq = { id: string; config: object } // package.config.set.dry
  export type DrySetPackageConfigRes = Breakages

  export type SetPackageConfigReq = WithExpire<DrySetPackageConfigReq> // package.config.set
  export type SetPackageConfigRes = WithRevision<null>

  export type RestorePackagesReq = WithExpire<{
    // package.backup.restore
    ids: string[]
    'target-id': string
    'old-password': string | null
    password: string
  }>
  export type RestorePackagesRes = WithRevision<null>

  export type ExecutePackageActionReq = {
    id: string
    'action-id': string
    input?: object
  } // package.action
  export type ExecutePackageActionRes = ActionResponse

  export type StartPackageReq = WithExpire<{ id: string }> // package.start
  export type StartPackageRes = WithRevision<null>

  export type DryStopPackageReq = StopPackageReq // package.stop.dry
  export type DryStopPackageRes = Breakages

  export type StopPackageReq = WithExpire<{ id: string }> // package.stop
  export type StopPackageRes = WithRevision<null>

  export type DryUninstallPackageReq = UninstallPackageReq // package.uninstall.dry
  export type DryUninstallPackageRes = Breakages

  export type UninstallPackageReq = WithExpire<{ id: string }> // package.uninstall
  export type UninstallPackageRes = WithRevision<null>

  export type DeleteRecoveredPackageReq = { id: string } // package.delete-recovered
  export type DeleteRecoveredPackageRes = WithRevision<null>

  export type DryConfigureDependencyReq = {
    'dependency-id': string
    'dependent-id': string
  } // package.dependency.configure.dry
  export type DryConfigureDependencyRes = {
    'old-config': object
    'new-config': object
    spec: ConfigSpec
  }

  // marketplace

  export type GetMarketplaceDataReq = { url?: string }
  export type GetMarketplaceDataRes = MarketplaceData

  export type GetMarketplaceEOSReq = {
    'eos-version-compat': string
  }
  export type GetMarketplaceEOSRes = MarketplaceEOS

  export type GetMarketplacePackagesReq = {
    ids?: { id: string; version: string }[]
    'eos-version-compat': string
    // iff !ids
    category?: string
    query?: string
    page?: string
    'per-page'?: string
  }
  export type GetMarketplacePackagesRes = MarketplacePkg[]

  export type GetReleaseNotesReq = { id: string }
  export type GetReleaseNotesRes = { [version: string]: string }
}

export type WithExpire<T> = { 'expire-id'?: string } & T
export type WithRevision<T> = { response: T; revision?: Revision }

export interface MarketplaceData {
  categories: string[]
  name: string
}

export interface MarketplaceEOS {
  version: string
  headline: string
  'release-notes': { [version: string]: string }
}

export interface MarketplacePkg {
  icon: URL
  license: URL
  instructions: URL
  manifest: Manifest
  categories: string[]
  versions: string[]
  'dependency-metadata': {
    [id: string]: {
      title: string
      icon: URL
    }
  }
}

export interface Breakages {
  [id: string]: TaggedDependencyError
}

export interface TaggedDependencyError {
  dependency: string
  error: DependencyError
}

export interface Log {
  timestamp: string
  message: string
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

export type BackupTarget = DiskBackupTarget | CifsBackupTarget

export interface EmbassyOSRecoveryInfo {
  version: string
  full: boolean
  'password-hash': string | null
  'wrapped-key': string | null
}

export interface DiskBackupTarget {
  type: 'disk'
  vendor: string | null
  model: string | null
  logicalname: string | null
  label: string | null
  capacity: number
  used: number | null
  'embassy-os': EmbassyOSRecoveryInfo | null
}

export interface CifsBackupTarget {
  type: 'cifs'
  hostname: string
  path: string
  username: string
  mountable: boolean
  'embassy-os': EmbassyOSRecoveryInfo | null
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

export interface DiskInfo {
  logicalname: string
  vendor: string | null
  model: string | null
  partitions: PartitionInfo[]
  capacity: number
  guid: string | null
}

export interface PartitionInfo {
  logicalname: string
  label: string | null
  capacity: number
  used: number | null
  'embassy-os': EmbassyOsDiskInfo | null
}

export interface EmbassyOsDiskInfo {
  version: string
  full: boolean
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
