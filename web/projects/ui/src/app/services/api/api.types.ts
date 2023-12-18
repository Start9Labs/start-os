import { Dump, Revision } from 'patch-db-client'
import { MarketplacePkg, StoreInfo, Manifest } from '@start9labs/marketplace'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import {
  DataModel,
  DomainInfo,
  NetworkStrategy,
  OsOutboundProxy,
  ServiceOutboundProxy,
  HealthCheckResult,
} from 'src/app/services/patch-db/data-model'
import {
  StartOSDiskInfo,
  FetchLogsReq,
  FetchLogsRes,
  FollowLogsRes,
  FollowLogsReq,
} from '@start9labs/shared'
import { customSmtp } from '@start9labs/start-sdk/lib/config/configConstants'

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
    'old-password': string
    'new-password': string
  } // auth.reset-password
  export type ResetPasswordRes = null

  // server

  export type EchoReq = { message: string; timeout?: number } // server.echo
  export type EchoRes = string

  export type GetSystemTimeReq = {} // server.time
  export type GetSystemTimeRes = {
    now: string
    uptime: number // seconds
  }

  export type GetServerLogsReq = FetchLogsReq // server.logs & server.kernel-logs & server.tor-logs
  export type GetServerLogsRes = FetchLogsRes

  export type FollowServerLogsReq = FollowLogsReq & { limit?: number } // server.logs.follow & server.kernel-logs.follow & server.tor-logs.follow
  export type FollowServerLogsRes = FollowLogsRes

  export type GetServerMetricsReq = {} // server.metrics
  export type GetServerMetricsRes = {
    guid: string
    metrics: Metrics
  }

  export type UpdateServerReq = { 'marketplace-url': string } // server.update
  export type UpdateServerRes = 'updating' | 'no-updates'

  export type SetServerClearnetAddressReq = { domainInfo: DomainInfo | null } // server.set-clearnet
  export type SetServerClearnetAddressRes = null

  export type RestartServerReq = {} // server.restart
  export type RestartServerRes = null

  export type ShutdownServerReq = {} // server.shutdown
  export type ShutdownServerRes = null

  export type SystemRebuildReq = {} // server.rebuild
  export type SystemRebuildRes = null

  export type ResetTorReq = {
    'wipe-state': boolean
    reason: string
  } // net.tor.reset
  export type ResetTorRes = null

  export type ToggleZramReq = {
    enable: boolean
  } // server.experimental.zram
  export type ToggleZramRes = null

  export type SetOsOutboundProxyReq = {
    proxy: OsOutboundProxy
  } // server.proxy.set-outbound
  export type SetOsOutboundProxyRes = null

  // sessions

  export type GetSessionsReq = {} // sessions.list
  export type GetSessionsRes = {
    current: string
    sessions: { [hash: string]: Session }
  }

  export type KillSessionsReq = { ids: string[] } // sessions.kill
  export type KillSessionsRes = null

  // notification

  export type FollowNotificationsReq = {}
  export type FollowNotificationsRes = {
    notifications: ServerNotifications
    guid: string
  }

  export type GetNotificationsReq = {
    before?: number
    limit?: number
  } // notification.list
  export type GetNotificationsRes = ServerNotification<number>[]

  export type DeleteNotificationReq = { ids: number[] } // notification.delete
  export type DeleteNotificationRes = null

  export type MarkSeenNotificationReq = DeleteNotificationReq // notification.mark-seen
  export type MarkSeenNotificationRes = null

  export type MarkSeenAllNotificationsReq = { before: number } // notification.mark-seen-before
  export type MarkSeenAllNotificationsRes = null

  export type MarkUnseenNotificationReq = DeleteNotificationReq // notification.mark-unseen
  export type MarkUnseenNotificationRes = null

  // network

  export type AddProxyReq = {
    name: string
    config: string
  } // net.proxy.add
  export type AddProxyRes = null

  export type UpdateProxyReq = {
    name?: string
    primaryInbound?: true
    primaryOutbound?: true
  } // net.proxy.update
  export type UpdateProxyRes = null

  export type DeleteProxyReq = { id: string } // net.proxy.delete
  export type DeleteProxyRes = null

  // domains

  export type ClaimStart9ToReq = { networkStrategy: NetworkStrategy } // net.domain.me.claim
  export type ClaimStart9ToRes = null

  export type DeleteStart9ToReq = {} // net.domain.me.delete
  export type DeleteStart9ToRes = null

  export type AddDomainReq = {
    hostname: string
    provider: {
      name: string
      username: string | null
      password: string | null
    }
    networkStrategy: NetworkStrategy
  } // net.domain.add
  export type AddDomainRes = null

  export type DeleteDomainReq = { hostname: string } // net.domain.delete
  export type DeleteDomainRes = null

  // port forwards

  export type OverridePortReq = { target: number; port: number } // net.port-forwards.override
  export type OverridePortRes = null

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

  // email

  export type ConfigureEmailReq = typeof customSmtp.validator._TYPE // email.configure
  export type ConfigureEmailRes = null

  export type TestEmailReq = ConfigureEmailReq & { to: string } // email.test
  export type TestEmailRes = null

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

  export type DeleteBackupRunsReq = { ids: string[] } // backup.runs.delete
  export type DeleteBackupRunsRes = null

  export type GetBackupInfoReq = { 'target-id': string; password: string } // backup.target.info
  export type GetBackupInfoRes = BackupInfo

  export type CreateBackupReq = { 'target-id': string; 'package-ids': string[] } // backup.create
  export type CreateBackupRes = null

  // package

  export type GetPackageCredentialsReq = { id: string } // package.credentials
  export type GetPackageCredentialsRes = Record<string, string>

  export type GetPackageLogsReq = FetchLogsReq & { id: string } // package.logs
  export type GetPackageLogsRes = FetchLogsRes

  export type FollowPackageLogsReq = FollowServerLogsReq & { id: string } // package.logs.follow
  export type FollowPackageLogsRes = FollowServerLogsRes

  export type InstallPackageReq = {
    id: string
    'version-spec'?: string
    'version-priority'?: 'min' | 'max'
    'marketplace-url': string
  } // package.install
  export type InstallPackageRes = null

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

  export type SetInterfaceClearnetAddressReq = SetServerClearnetAddressReq & {
    packageId: string
    interfaceId: string
  } // package.interface.set-clearnet
  export type SetInterfaceClearnetAddressRes = null

  export type SetServiceOutboundProxyReq = {
    packageId: string
    proxy: ServiceOutboundProxy
  } // package.proxy.set-outbound
  export type SetServiceOutboundProxyRes = null

  // marketplace

  export type GetMarketplaceInfoReq = { 'server-id': string }
  export type GetMarketplaceInfoRes = StoreInfo

  export type GetMarketplaceEosReq = { 'server-id': string }
  export type GetMarketplaceEosRes = MarketplaceEOS

  export type GetMarketplacePackagesReq = {
    ids?: { id: string; version: string }[]
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
    'percentage-used': MetricData
    used: MetricData
    available: MetricData
    'zram-total': MetricData
    'zram-used': MetricData
    'zram-available': MetricData
  }
  cpu: {
    'percentage-used': MetricData
    idle: MetricData
    'user-space': MetricData
    'kernel-space': MetricData
    wait: MetricData
  }
  disk: {
    capacity: MetricData
    'percentage-used': MetricData
    used: MetricData
    available: MetricData
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
  'embassy-os': StartOSDiskInfo | null
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

export type ServerNotifications = ServerNotification<number>[]

export interface ServerNotification<T extends number> {
  id: number
  'package-id': string | null
  'created-at': string
  code: T
  level: NotificationLevel
  title: string
  message: string
  data: NotificationData<T>
  read: boolean
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

export interface DependencyErrorTransitive {
  type: DependencyErrorType.Transitive
}