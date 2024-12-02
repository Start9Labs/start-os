import {
  DomainInfo,
  NetworkStrategy,
} from 'src/app/services/patch-db/data-model'
import { FetchLogsReq, FetchLogsRes, Log } from '@start9labs/shared'
import { Dump } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StartOSDiskInfo } from '@start9labs/shared'
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

  export type InitFollowProgressRes = {
    progress: T.FullProgress
    guid: string
  }

  // server

  export type GetSystemTimeReq = {} // server.time
  export type GetSystemTimeRes = {
    now: string
    uptime: number // seconds
  }

  export type GetServerLogsReq = FetchLogsReq // server.logs & server.kernel-logs & server.tor-logs
  export type GetServerLogsRes = FetchLogsRes

  export type FollowServerLogsReq = {
    limit?: number // (optional) default is 50. Ignored if cursor provided
    boot?: number | string | null // (optional) number is offset (0: current, -1 prev, +1 first), string is a specific boot id, null is all. Default is undefined
    cursor?: string // the last known log. Websocket will return all logs since this log
  } // server.logs.follow & server.kernel-logs.follow
  export type FollowServerLogsRes = {
    startCursor: string
    guid: string
  }

  export type FollowServerMetricsReq = {} // server.metrics.follow
  export type FollowServerMetricsRes = {
    guid: string
    metrics: ServerMetrics
  }

  export type UpdateServerReq = { registry: string } // server.update
  export type UpdateServerRes = 'updating' | 'no-updates'

  export type SetServerClearnetAddressReq = { domainInfo: DomainInfo | null } // server.set-clearnet
  export type SetServerClearnetAddressRes = null

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

  export type SetOsOutboundProxyReq = {
    proxy: string | null
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
    name: string
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

  export type EnableWifiReq = { enable: boolean } // wifi.enable
  export type EnableWifiRes = null

  export type ConnectWifiReq = { ssid: string } // wifi.connect
  export type ConnectWifiRes = null

  export type DeleteWifiReq = { ssid: string } // wifi.delete
  export type DeleteWifiRes = null

  // email

  export type ConfigureEmailReq = T.SmtpValue // email.configure
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
    unknownDisks: UnknownDisk[]
    saved: Record<string, BackupTarget>
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
  export type AddBackupTargetRes = Record<string, BackupTarget>

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
    targetId: string
    cron: string
    packageIds: string[]
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

  export type GetBackupInfoReq = { targetId: string; password: string } // backup.target.info
  export type GetBackupInfoRes = BackupInfo

  export type CreateBackupReq = { targetId: string; packageIds: string[] } // backup.create
  export type CreateBackupRes = null

  // package
  // @TODO Matt I just copy-pasted those types from minor
  export type GetPackageLogsReq = {
    id: string
    before: boolean
    cursor?: string
    limit?: number
  } // package.logs
  export type GetPackageLogsRes = {
    entries: Log[]
    startCursor?: string
    endCursor?: string
  }

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
    size: number // bytes
  }
  export type SideloadPackageRes = {
    upload: string
    progress: string
  }

  export type SetInterfaceClearnetAddressReq = SetServerClearnetAddressReq & {
    packageId: string
    interfaceId: string
  } // package.interface.set-clearnet
  export type SetInterfaceClearnetAddressRes = null

  export type SetServiceOutboundProxyReq = {
    packageId: string
    proxy: string | null
  } // package.proxy.set-outbound
  export type SetServiceOutboundProxyRes = null

  // registry

  /** these are returned in ASCENDING order. the newest available version will be the LAST in the object */
  export type GetRegistryOsUpdateRes = { [version: string]: T.OsVersionInfo }

  export type CheckOSUpdateReq = { serverId: string }
  export type CheckOSUpdateRes = OSUpdate
}

export type OSUpdate = {
  version: string
  headline: string
  releaseNotes: { [version: string]: string }
}

export type Breakages = {
  [id: string]: TaggedDependencyError
}

export type TaggedDependencyError = {
  dependency: string
  error: DependencyError
}

interface MetricData {
  value: string
  unit: string
}

export type ServerMetrics = {
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

export type AppMetrics = {
  memory: {
    percentageUsed: MetricData
    used: MetricData
  }
  cpu: {
    percentageUsed: MetricData
  }
  disk: {
    percentageUsed: MetricData
    used: MetricData
  }
}

export type Session = {
  loggedIn: string
  lastActive: string
  userAgent: string
  metadata: SessionMetadata
}

export type SessionMetadata = {
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
  startOs: Record<string, StartOSDiskInfo>
}

export interface BaseBackupTarget {
  type: BackupTargetType
  name: string
  mountable: boolean
  path: string
  startOs: Record<string, StartOSDiskInfo>
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

export type BackupRun = {
  id: string
  startedAt: string
  completedAt: string
  packageIds: string[]
  job: BackupJob
  report: BackupReport
}

export type BackupJob = {
  id: string
  name: string
  targetId: string
  cron: string // '* * * * * *' https://cloud.google.com/scheduler/docs/configuring/cron-job-schedules
  packageIds: string[]
}

export type BackupInfo = {
  version: string
  timestamp: string
  packageBackups: {
    [id: string]: PackageBackupInfo
  }
}

export type PackageBackupInfo = {
  title: string
  version: string
  osVersion: string
  timestamp: string
}

export type ServerSpecs = {
  [key: string]: string | number
}

export type SSHKey = {
  createdAt: string
  alg: string
  hostname: string
  fingerprint: string
}

export type ServerNotifications = ServerNotification<number>[]

export type ServerNotification<T extends number> = {
  id: number
  packageId: string | null
  createdAt: string
  code: T
  level: 'success' | 'info' | 'warning' | 'error'
  title: string
  message: string
  data: NotificationData<T>
  read: boolean
}

export type NotificationData<T> = T extends 0
  ? null
  : T extends 1
    ? BackupReport
    : T extends 2
      ? string
      : any

export type BackupReport = {
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

export type AvailableWifi = {
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
  | DependencyErrorActionRequired
  | DependencyErrorHealthChecksFailed
  | DependencyErrorTransitive

export type DependencyErrorNotInstalled = {
  type: 'notInstalled'
}

export type DependencyErrorNotRunning = {
  type: 'notRunning'
}

export type DependencyErrorIncorrectVersion = {
  type: 'incorrectVersion'
  expected: string // version range
  received: string // version
}

export interface DependencyErrorActionRequired {
  type: 'actionRequired'
}

export type DependencyErrorHealthChecksFailed = {
  type: 'healthChecksFailed'
  check: T.NamedHealthCheckResult
}

export type DependencyErrorTransitive = {
  type: 'transitive'
}
