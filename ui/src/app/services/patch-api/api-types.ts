import { ConfigSpec } from 'src/app/app-config/config-types'
import { AppAvailableFull, AppAvailablePreview, AppAvailableVersionSpecificInfo, AppInstalledFull, AppInstalledPreview, DependentBreakage } from 'src/app/models/app-types'
import { AppMetricsVersioned } from 'src/app/util/metrics.util'
import { Rules } from '../../models/app-model'
import { SSHFingerprint, ServerStatus, ServerSpecs, DiskInfo, ServerMetrics, S9Notification } from '../../models/server-model'

/** SERVER **/

export interface ApiServer {
  name: string
  status: ServerStatus
  versionInstalled: string
  alternativeRegistryUrl: string | null
  specs: ServerSpecs
  wifi: {
    ssids: string[]
    current: string | null
  }
  ssh: SSHFingerprint[]
  serverId: string
  welcomeAck: boolean
  autoCheckUpdates: boolean
}

/** APPS **/
export type ApiAppAvailableFull = Omit<AppAvailableFull, 'versionViewing'>

export type ApiAppInstalledPreview = Omit<AppInstalledPreview, 'hasUI' | 'launchable'>
export type ApiAppInstalledFull = Omit<AppInstalledFull, 'hasFetchedFull' | 'hasUI' | 'launchable'>

export interface ApiAppConfig {
  spec: ConfigSpec
  config: object | null
  rules: Rules[]
}

/** MISC **/

export type Unit = { never?: never; } // hack for the unit typ

// Can add types here if inlining them is too fat.
export module ReqRes {
  export type GetVersionRes = { version: string }
  export type PostLoginReq = { password: string }
  export type PostLoginRes = Unit
  export type ServiceActionRequest = {
    jsonrpc: '2.0',
    id: string,
    method: string
  }
  export type ServiceActionResponse = {
    jsonrpc: '2.0',
    id: string
  } & ({ error: { code: number, message: string } } | { result : string })
  export type GetCheckAuthRes = { }
  export type GetServerRes = ApiServer
  export type GetVersionLatestRes = { versionLatest: string, releaseNotes: string }
  export type GetServerMetricsRes = ServerMetrics
  export type GetAppAvailableRes = ApiAppAvailableFull
  export type GetAppAvailableVersionInfoRes = AppAvailableVersionSpecificInfo
  export type GetAppsAvailableRes = AppAvailablePreview[]
  export type GetExternalDisksRes = DiskInfo[]
  export type GetAppInstalledRes = ApiAppInstalledFull
  export type GetAppConfigRes = ApiAppConfig
  export type GetAppLogsReq = { after?: string, before?: string, page?: string, perPage?: string }
  export type GetServerLogsReq = { }
  export type GetAppLogsRes = string[]
  export type GetServerLogsRes = string
  export type GetAppMetricsRes = AppMetricsVersioned<number>
  export type GetAppsInstalledRes = ApiAppInstalledPreview[]
  export type PostInstallAppReq = { version: string }
  export type PostInstallAppRes = ApiAppInstalledFull & { breakages: DependentBreakage[] }
  export type PostUpdateAgentReq = { version: string }
  export type PostAppBackupCreateReq = { logicalname: string, password: string }
  export type PostAppBackupCreateRes = Unit
  export type PostAppBackupRestoreReq = { logicalname: string, password: string }
  export type PostAppBackupRestoreRes = Unit
  export type PostAppBackupStopRes = Unit
  export type PatchAppConfigReq = { config: object }
  export type PatchServerConfigReq = { value: string }
  export type GetNotificationsReq = { page: string, perPage: string }
  export type GetNotificationsRes = S9Notification[]
  export type PostAddWifiReq = { ssid: string, password: string, country: string, skipConnect: boolean }
  export type PostConnectWifiReq = { country: string }
  export type PostAddSSHKeyReq = { sshKey: string }
  export type PostAddSSHKeyRes = SSHFingerprint
}

