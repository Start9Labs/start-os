import { Dump } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StartOSDiskInfo, LogsRes, ServerLogsReq } from '@start9labs/shared'
import { IST, T } from '@start9labs/start-sdk'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'

export module RR {
  // websocket

  export type WebsocketConfig<T> = Omit<WebSocketSubjectConfig<T>, 'url'>

  // state

  export type EchoReq = { message: string } // server.echo
  export type EchoRes = string

  export type ServerState = 'initializing' | 'error' | 'running'

  // DB

  export type SubscribePatchReq = {}
  export type SubscribePatchRes = {
    dump: Dump<DataModel>
    guid: string
  }

  export type SetDBValueReq<T> = { pointer: string; value: T } // db.put.ui
  export type SetDBValueRes = null

  // auth

  export type LoginReq = {
    password: string
    metadata: SessionMetadata
    ephemeral?: boolean
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

  // init

  export type InitGetProgressRes = {
    progress: T.FullProgress
    guid: string
  }

  // server

  export type GetSystemTimeReq = {} // server.time
  export type GetSystemTimeRes = {
    now: string
    uptime: number // seconds
  }

  export type GetServerLogsReq = ServerLogsReq // server.logs & server.kernel-logs
  export type GetServerLogsRes = LogsRes

  export type FollowServerLogsReq = {
    limit?: number // (optional) default is 50. Ignored if cursor provided
    boot?: number | string | null // (optional) number is offset (0: current, -1 prev, +1 first), string is a specific boot id, null is all. Default is undefined
    cursor?: string // the last known log. Websocket will return all logs since this log
  } // server.logs.follow & server.kernel-logs.follow
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

  // smtp

  export type SetSMTPReq = T.SmtpValue // server.set-smtp
  export type SetSMTPRes = null

  export type ClearSMTPReq = {} // server.clear-smtp
  export type ClearSMTPRes = null

  export type TestSMTPReq = SetSMTPReq & { to: string } // server.test-smtp
  export type TestSMTPRes = null

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

  export type GetBackupInfoReq = {
    // backup.target.info
    targetId: string
    serverId: string
    password: string
  }
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

  export type GetPackageLogsReq = ServerLogsReq & { id: string } // package.logs
  export type GetPackageLogsRes = LogsRes

  export type FollowPackageLogsReq = FollowServerLogsReq & { id: string } // package.logs.follow
  export type FollowPackageLogsRes = FollowServerLogsRes

  export type InstallPackageReq = T.InstallParams
  export type InstallPackageRes = null

  export type GetActionInputReq = { packageId: string; actionId: string } // package.action.get-input
  export type GetActionInputRes = {
    spec: IST.InputSpec
    value: object | null
  }

  export type ActionReq = {
    packageId: string
    actionId: string
    input: object | null
  } // package.action.run
  export type ActionRes = (T.ActionResult & { version: '1' }) | null

  export type RestorePackagesReq = {
    // package.backup.restore
    ids: string[]
    targetId: string
    serverId: string
    password: string
  }
  export type RestorePackagesRes = null

  export type StartPackageReq = { id: string } // package.start
  export type StartPackageRes = null

  export type RestartPackageReq = { id: string } // package.restart
  export type RestartPackageRes = null

  export type StopPackageReq = { id: string } // package.stop
  export type StopPackageRes = null

  export type RebuildPackageReq = { id: string } // package.rebuild
  export type RebuildPackageRes = null

  export type UninstallPackageReq = { id: string } // package.uninstall
  export type UninstallPackageRes = null

  export type SideloadPackageReq = {
    manifest: T.Manifest
    icon: string // base64
  }
  export type SideloadPackageRes = {
    upload: string // guid
    progress: string // guid
  }

  // registry

  /** these are returned in ASCENDING order. the newest available version will be the LAST in the object */
  export type GetRegistryOsUpdateRes = { [version: string]: T.OsVersionInfo }

  export type CheckOSUpdateReq = { serverId: string }
  export type CheckOSUpdateRes = OSUpdate
}

export interface OSUpdate {
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
  loggedIn: string
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
  startOs: Record<string, StartOSDiskInfo>
}

export interface CifsBackupTarget {
  type: 'cifs'
  hostname: string
  path: string
  username: string
  mountable: boolean
  startOs: Record<string, StartOSDiskInfo>
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
  : T extends 2
  ? string
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
  | DependencyErrorActionRequired
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

export interface DependencyErrorActionRequired {
  type: 'actionRequired'
}

export interface DependencyErrorHealthChecksFailed {
  type: 'healthChecksFailed'
  check: T.NamedHealthCheckResult
}

export interface DependencyErrorTransitive {
  type: 'transitive'
}
