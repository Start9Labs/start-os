import { Rules } from '../../models/app-model'
import { AppAvailablePreview, AppAvailableFull, AppInstalledPreview, AppInstalledFull, DependentBreakage, AppAvailableVersionSpecificInfo, ServiceAction } from '../../models/app-types'
import { S9Notification, SSHFingerprint, DiskInfo } from '../../models/server-model'
import { Subject, Observable } from 'rxjs'
import { Unit, ApiServer, ReqRes } from './api-types'
import { AppMetrics } from 'src/app/util/metrics.util'
import { ConfigSpec } from 'src/app/app-config/config-types'
import { SeqUpdate, Source } from 'patch-db-client'
import { DataModel } from 'src/app/models/patch-db/data-model'
import { filter } from 'rxjs/operators'

export type PatchPromise<T> = Promise<{ response: T, patch: SeqUpdate<DataModel> }>

export abstract class ApiService implements Source<DataModel>{
  private readonly patch = new Subject<SeqUpdate<DataModel>>()
  private syncing = true

  watch(): Observable<SeqUpdate<DataModel>> {
    return this.patch.asObservable().pipe(filter(() => this.syncing))
  }
  start(): void { this.syncing = true }
  stop(): void { this.syncing = false }

  private $unauthorizedApiResponse$: Subject<{ }> = new Subject()
  constructor(){}

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
  abstract getVersionLatest (): Promise<ReqRes.GetVersionLatestRes>
  abstract getServerMetrics (): Promise<ReqRes.GetServerMetricsRes>
  abstract getNotifications (page: number, perPage: number): Promise<S9Notification[]>
  abstract deleteNotification (id: string): Promise<Unit>
  abstract getAvailableApps (): Promise<AppAvailablePreview[]>
  abstract getAvailableApp (appId: string): Promise<AppAvailableFull>
  abstract getAvailableAppVersionSpecificInfo (appId: string, versionSpec: string): Promise<AppAvailableVersionSpecificInfo>
  abstract getInstalledApp (appId: string): Promise<AppInstalledFull>
  abstract getAppMetrics (appId: string): Promise<AppMetrics>
  abstract getInstalledApps (): Promise<AppInstalledPreview[]>
  abstract getExternalDisks (): Promise<DiskInfo[]>
  abstract getAppConfig (appId: string): Promise<{ spec: ConfigSpec, config: object, rules: Rules[]}>
  abstract getAppLogs (appId: string, params?: ReqRes.GetAppLogsReq): Promise<string[]>
  abstract getServerLogs (): Promise<string>
  abstract getServer (): Promise<ApiServer>

  /** Any request which mutates state will return a PatchPromise: a patch to state along with the standard response. The syncResponse helper function syncs the patch and returns the response*/
  protected abstract toggleAppLANRaw (appId: string, toggle: 'enable' | 'disable'): PatchPromise<Unit>
  toggleAppLAN = this.syncResponse(this.toggleAppLANRaw)

  protected abstract updateAgentRaw (version: any): PatchPromise<Unit>
  updateAgent = this.syncResponse(this.updateAgentRaw)

  protected abstract acknowledgeOSWelcomeRaw (version: string): PatchPromise<Unit>
  acknowledgeOSWelcome = this.syncResponse(this.acknowledgeOSWelcomeRaw)

  protected abstract installAppRaw (appId: string, version: string, dryRun?: boolean): PatchPromise<AppInstalledFull & { breakages: DependentBreakage[] }  >
  installApp = this.syncResponse(this.installAppRaw)

  protected abstract uninstallAppRaw (appId: string, dryRun?: boolean): PatchPromise<{ breakages: DependentBreakage[] }>
  uninstallApp = this.syncResponse(this.uninstallAppRaw)

  protected abstract startAppRaw (appId: string): PatchPromise<Unit>
  startApp = this.syncResponse(this.startAppRaw)

  protected abstract stopAppRaw (appId: string, dryRun?: boolean): PatchPromise<{ breakages: DependentBreakage[] }>
  stopApp = this.syncResponse(this.stopAppRaw)

  protected abstract restartAppRaw (appId: string): PatchPromise<Unit>
  restartApp = this.syncResponse(this.restartAppRaw)

  protected abstract createAppBackupRaw (appId: string, logicalname: string, password?: string): PatchPromise<Unit>
  createAppBackup = this.syncResponse(this.createAppBackupRaw)

  protected abstract restoreAppBackupRaw (appId: string, logicalname: string, password?: string): PatchPromise<Unit>
  restoreAppBackup = this.syncResponse(this.restoreAppBackupRaw)

  protected abstract stopAppBackupRaw (appId: string): PatchPromise<Unit>
  stopAppBackup = this.syncResponse(this.stopAppBackupRaw)

  protected abstract patchAppConfigRaw (app: AppInstalledPreview, config: object, dryRun?: boolean): PatchPromise<{ breakages: DependentBreakage[] }>
  patchAppConfig = this.syncResponse(this.patchAppConfigRaw)

  protected abstract postConfigureDependencyRaw (dependencyId: string, dependentId: string, dryRun?: boolean): PatchPromise< { config: object, breakages: DependentBreakage[] }>
  postConfigureDependency = this.syncResponse(this.postConfigureDependencyRaw)

  protected abstract patchServerConfigRaw (attr: string, value: any): PatchPromise<Unit>
  patchServerConfig = this.syncResponse(this.patchServerConfigRaw)

  protected abstract wipeAppDataRaw (app: AppInstalledPreview): PatchPromise<Unit>
  wipeAppData = this.syncResponse(this.wipeAppDataRaw)

  protected abstract addSSHKeyRaw (sshKey: string): PatchPromise<Unit>
  addSSHKey = this.syncResponse(this.addSSHKeyRaw)

  protected abstract deleteSSHKeyRaw (sshKey: SSHFingerprint): PatchPromise<Unit>
  deleteSSHKey = this.syncResponse(this.deleteSSHKeyRaw)

  protected abstract addWifiRaw (ssid: string, password: string, country: string, connect: boolean): PatchPromise<Unit>
  addWifi = this.syncResponse(this.addWifiRaw)

  protected abstract connectWifiRaw (ssid: string): PatchPromise<Unit>
  connectWifi = this.syncResponse(this.connectWifiRaw)

  protected abstract deleteWifiRaw (ssid: string): PatchPromise<Unit>
  deleteWifi = this.syncResponse(this.deleteWifiRaw)

  protected abstract restartServerRaw (): PatchPromise<Unit>
  restartServer = this.syncResponse(this.restartServerRaw)

  protected abstract shutdownServerRaw (): PatchPromise<Unit>
  shutdownServer = this.syncResponse(this.shutdownServerRaw)

  protected abstract ejectExternalDiskRaw (logicalName: string): PatchPromise<Unit>
  ejectExternalDisk = this.syncResponse(this.ejectExternalDiskRaw)

  protected abstract serviceActionRaw (appId: string, serviceAction: ServiceAction): PatchPromise<ReqRes.ServiceActionResponse>
  serviceAction = this.syncResponse(this.serviceActionRaw)

  // helper allowing quick decoration to sync a patch and return a resposne.
  private syncResponse<T extends (...args: any[]) => PatchPromise<any>>(f: T): (...args: Parameters<T>) => ToPromise<ReturnType<T>> {
    return (...a) => f(a).then(({ response, patch }) => {
      this.patch.next(patch)
      return response
    }) as any
  }
}
// used for type inference in syncResponse
type ToPromise<T extends PatchPromise<any>> = T extends PatchPromise<infer R> ? Promise<R> : any


export function isRpcFailure<Error, Result> (arg: { error: Error } | { result: Result}): arg is { error: Error } {
  return !!(arg as any).error
}

export function isRpcSuccess<Error, Result> (arg: { error: Error } | { result: Result}): arg is { result: Result } {
  return !!(arg as any).result
}
