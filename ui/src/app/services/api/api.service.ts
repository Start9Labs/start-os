import { Rules } from '../../models/app-model'
import { AppAvailablePreview, AppAvailableFull, AppInstalledPreview, AppInstalledFull, DependentBreakage, AppAvailableVersionSpecificInfo, ServiceAction } from '../../models/app-types'
import { S9Notification, SSHFingerprint, ServerMetrics, DiskInfo } from '../../models/server-model'
import { Subject, Observable } from 'rxjs'
import { Unit, ApiServer, ApiAppInstalledFull, ApiAppConfig, ApiAppAvailableFull, ApiAppInstalledPreview } from './api-types'
import { AppMetrics, AppMetricsVersioned } from 'src/app/util/metrics.util'
import { ConfigSpec } from 'src/app/app-config/config-types'

export abstract class ApiService {
  private $unauthorizedApiResponse$: Subject<{ }> = new Subject()

  watch401$ (): Observable<{ }> {
    return this.$unauthorizedApiResponse$.asObservable()
  }

  authenticatedRequestsEnabled: boolean = false

  protected received401 () {
    this.authenticatedRequestsEnabled = false
    this.$unauthorizedApiResponse$.next()
  }

  abstract testConnection (url: string): Promise<true>
  abstract getCheckAuth (): Promise<Unit> // Throws an error on failed auth.
  abstract postLogin (password: string): Promise<Unit> // Throws an error on failed auth.
  abstract postLogout (): Promise<Unit> // Throws an error on failed auth.
  abstract getServer (timeout?: number): Promise<ApiServer>
  abstract getVersionLatest (): Promise<ReqRes.GetVersionLatestRes>
  abstract getServerMetrics (): Promise<ReqRes.GetServerMetricsRes>
  abstract getNotifications (page: number, perPage: number): Promise<S9Notification[]>
  abstract deleteNotification (id: string): Promise<Unit>
  abstract toggleAppLAN (appId: string, toggle: 'enable' | 'disable'): Promise<Unit>
  abstract updateAgent (version: any): Promise<Unit>
  abstract acknowledgeOSWelcome (version: string): Promise<Unit>
  abstract getAvailableApps (): Promise<AppAvailablePreview[]>
  abstract getAvailableApp (appId: string): Promise<AppAvailableFull>
  abstract getAvailableAppVersionSpecificInfo (appId: string, versionSpec: string): Promise<AppAvailableVersionSpecificInfo>
  abstract getInstalledApp (appId: string): Promise<AppInstalledFull>
  abstract getAppMetrics (appId: string): Promise<AppMetrics>
  abstract getInstalledApps (): Promise<AppInstalledPreview[]>
  abstract getExternalDisks (): Promise<DiskInfo[]>
  abstract getAppConfig (appId: string): Promise<{ spec: ConfigSpec, config: object, rules: Rules[] }>
  abstract getAppLogs (appId: string, params?: ReqRes.GetAppLogsReq): Promise<string[]>
  abstract getServerLogs (): Promise<string[]>
  abstract installApp (appId: string, version: string, dryRun?: boolean): Promise<AppInstalledFull & { breakages: DependentBreakage[] }>
  abstract uninstallApp (appId: string, dryRun?: boolean): Promise<{ breakages: DependentBreakage[] }>
  abstract startApp (appId: string): Promise<Unit>
  abstract stopApp (appId: string, dryRun?: boolean): Promise<{ breakages: DependentBreakage[] }>
  abstract restartApp (appId: string): Promise<Unit>
  abstract createAppBackup (appId: string, logicalname: string, password?: string): Promise<Unit>
  abstract restoreAppBackup (appId: string, logicalname: string, password?: string): Promise<Unit>
  abstract stopAppBackup (appId: string): Promise<Unit>
  abstract patchAppConfig (app: AppInstalledPreview, config: object, dryRun?: boolean): Promise<{ breakages: DependentBreakage[] }>
  abstract postConfigureDependency (dependencyId: string, dependentId: string, dryRun?: boolean): Promise<{ config: object, breakages: DependentBreakage[] }>
  abstract patchServerConfig (attr: string, value: any): Promise<Unit>
  abstract wipeAppData (app: AppInstalledPreview): Promise<Unit>
  abstract addSSHKey (sshKey: string): Promise<Unit>
  abstract deleteSSHKey (sshKey: SSHFingerprint): Promise<Unit>
  abstract addWifi (ssid: string, password: string, country: string, connect: boolean): Promise<Unit>
  abstract connectWifi (ssid: string): Promise<Unit>
  abstract deleteWifi (ssid: string): Promise<Unit>
  abstract restartServer (): Promise<Unit>
  abstract shutdownServer (): Promise<Unit>
  abstract ejectExternalDisk (logicalName: string): Promise<Unit>
  abstract serviceAction (appId: string, serviceAction: ServiceAction): Promise<ReqRes.ServiceActionResponse>
  abstract refreshLAN (): Promise<Unit>
}

export function isRpcFailure<Error, Result> (arg: { error: Error } | { result: Result }): arg is { error: Error } {
  return !!(arg as any).error
}

export function isRpcSuccess<Error, Result> (arg: { error: Error } | { result: Result }): arg is { result: Result } {
  return !!(arg as any).result
}

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
  } & ({ error: { code: number, message: string } } | { result: string })
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
  export type GetServerLogsRes = string[]
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

