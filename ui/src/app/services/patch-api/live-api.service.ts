import { Injectable } from '@angular/core'
import { HttpService, Method, HttpOptions } from '../http.service'
import { AppAvailablePreview, AppAvailableFull, AppInstalledFull, AppInstalledPreview, DependentBreakage, AppAvailableVersionSpecificInfo, ServiceAction } from '../../models/app-types'
import { S9Notification, SSHFingerprint, DiskInfo } from '../../models/server-model'
import { ApiService, PatchPromise  } from './api.service'
import { ApiServer, ReqRes, Unit } from './api-types'
import { HttpErrorResponse } from '@angular/common/http'
import { isUnauthorized } from 'src/app/util/web.util'
import { Replace } from 'src/app/util/types.util'
import { AppMetrics, parseMetricsPermissive } from 'src/app/util/metrics.util'
import { Observable, of, throwError } from 'rxjs'
import { catchError, mapTo } from 'rxjs/operators'
import * as uuid from 'uuid'
import { SeqPatch, SeqReplace } from 'patch-db-client'
import { DataModel } from 'src/app/models/patch-db/data-model'

@Injectable()
export class LiveApiService extends ApiService {
  constructor (
    private readonly http: HttpService,
  ) { super() }

  async getUpdates(startSequence: number, finishSequence?: number): Promise<SeqPatch[]> {
    const queryParams = `start=${startSequence}` + (finishSequence ? `&finish=${finishSequence}` : ``)
    return this.authRequest({ method: Method.GET, url: `/revisions?${queryParams}` })
  }

  async getDump(): Promise<SeqReplace<DataModel>> {
    return this.authRequest({ method: Method.GET, url: `/revisions?dump` })
  }

  // Used to check whether password or key is valid. If so, it will be used implicitly by all other calls.
  async getCheckAuth (): Promise<Unit> {
    return this.http.serverRequest<Unit>({ method: Method.GET, url: '/authenticate' }, { version: '' })
  }

  async postLogin (password: string): Promise<Unit> {
    return this.http.serverRequest<Unit>({ method: Method.POST, url: '/auth/login', data: { password } }, { version: '' })
  }

  async postLogout (): Promise<Unit> {
    return this.http.serverRequest<Unit>({ method: Method.POST, url: '/auth/logout' }, { version: '' }).then(() => { this.authenticatedRequestsEnabled = false; return { } })
  }

  async getServer (timeout?: number): Promise<ApiServer> {
    return this.authRequest<any>({ method: Method.GET, url: '/', readTimeout: timeout })
  }

  async getVersionLatest (): Promise<ReqRes.GetVersionLatestRes> {
    return this.authRequest({ method: Method.GET, url: '/versionLatest' }, { version: '' })
  }

  async getServerMetrics (): Promise<ReqRes.GetServerMetricsRes> {
    return this.authRequest({ method: Method.GET, url: `/metrics` })
  }

  async getNotifications (page: number, perPage: number): Promise<S9Notification[]> {
    const params: ReqRes.GetNotificationsReq = {
      page: String(page),
      perPage: String(perPage),
    }
    return this.authRequest({ method: Method.GET, url: `/notifications`, params })
  }

  async deleteNotificationRaw (id: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.DELETE, url: `/notifications/${id}` })
  }

  async getExternalDisks (): Promise<DiskInfo[]> {
    return this.authRequest<ReqRes.GetExternalDisksRes>({ method: Method.GET, url: `/disks` })
  }

  // TODO: EJECT-DISKS
  async ejectExternalDiskRaw (logicalName: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/disks/eject`, data: { logicalName } })
  }

  async updateAgentRaw (version: string): PatchPromise<Unit> {
    const data: ReqRes.PostUpdateAgentReq = {
      version: `=${version}`,
    }
    return this.authRequest({ method: Method.POST, url: '/update', data })
  }

  async getAvailableAppVersionSpecificInfo (appId: string, versionSpec: string): Promise<AppAvailableVersionSpecificInfo> {
    return this
      .authRequest<Replace<ReqRes.GetAppAvailableVersionInfoRes, 'versionViewing', 'version'>>( { method: Method.GET, url: `/apps/${appId}/store/${versionSpec}` })
      .then( res => ({ ...res, versionViewing: res.version }))
      .then( res => {
        delete res['version']
        return res
       })
  }

  async getAvailableApps (): Promise<AppAvailablePreview[]> {
    const res = await this.authRequest<ReqRes.GetAppsAvailableRes>({ method: Method.GET, url: '/apps/store' })
    return res.map(a => {
      const latestVersionTimestamp = new Date(a.latestVersionTimestamp)
      if (isNaN(latestVersionTimestamp as any)) throw new Error(`Invalid latestVersionTimestamp ${a.latestVersionTimestamp}`)
      return { ...a, latestVersionTimestamp }
    })
  }

  async getAvailableApp (appId: string): Promise<AppAvailableFull> {
    return this.authRequest<ReqRes.GetAppAvailableRes>({ method: Method.GET, url: `/apps/${appId}/store` })
      .then(res => {
        return {
          ...res,
          versionViewing: res.versionLatest,
        }
      })
  }

  async getInstalledApp (appId: string): Promise<AppInstalledFull> {
    return this.authRequest<ReqRes.GetAppInstalledRes>({ method: Method.GET, url: `/apps/${appId}/installed` })
      .then(app => ({ ...app, hasFetchedFull: true }))
  }

  async getAppMetrics (appId: string): Promise<AppMetrics> {
    return this.authRequest<ReqRes.GetAppMetricsRes | string>( { method: Method.GET, url: `/apps/${appId}/metrics` })
      .then(parseMetricsPermissive)
  }

  async getInstalledApps (): Promise<AppInstalledPreview[]> {
    return this.authRequest<ReqRes.GetAppsInstalledRes>({ method: Method.GET, url: `/apps/installed` })
  }

  async getAppConfig ( appId: string): Promise<ReqRes.GetAppConfigRes> {
    return this.authRequest<ReqRes.GetAppConfigRes>({ method: Method.GET, url: `/apps/${appId}/config` })
  }

  async getAppLogs (appId: string, params: ReqRes.GetAppLogsReq = { }): Promise<string[]> {
    return this.authRequest<ReqRes.GetAppLogsRes>( { method: Method.GET, url: `/apps/${appId}/logs`, params: params as any })
  }

  async getServerLogs (): Promise<string> {
    return this.authRequest<ReqRes.GetServerLogsRes>( { method: Method.GET, url: `/logs` })
  }

  async testConnection (url: string): Promise<true> {
    return this.http.raw.get(url).pipe(mapTo(true as true), catchError(catchHttpStatusError)).toPromise()
  }

  async acknowledgeOSWelcomeRaw (version: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/welcome/${version}` })
  }

  async installAppRaw (appId: string, version: string, dryRun: boolean = false): PatchPromise<AppInstalledFull & { breakages: DependentBreakage[] }> {
    const data: ReqRes.PostInstallAppReq = {
      version,
    }
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/install${dryRunParam(dryRun, true)}`, data })
      .then(({ patch, response }) => ({ patch, response: { ...response, hasFetchedFull: false }}))
  }

  async uninstallAppRaw (appId: string, dryRun: boolean = false): PatchPromise<{ breakages: DependentBreakage[] }> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/uninstall${dryRunParam(dryRun, true)}`, readTimeout: 60000 })
  }

  async startAppRaw (appId: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/start`, readTimeout: 60000 })
  }

  async stopAppRaw (appId: string, dryRun: boolean = false): PatchPromise<{ breakages: DependentBreakage[] }> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/stop${dryRunParam(dryRun, true)}`, readTimeout: 60000 })
  }

  async restartAppRaw (appId: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/restart`, readTimeout: 60000 })
  }

  async createAppBackupRaw (appId: string, logicalname: string, password?: string): PatchPromise<Unit> {
    const data: ReqRes.PostAppBackupCreateReq = {
      password: password || undefined,
      logicalname,
    }
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/backup`, data, readTimeout: 60000 })
  }

  async stopAppBackupRaw (appId: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/backup/stop`, readTimeout: 60000 })
  }

  async restoreAppBackupRaw (appId: string, logicalname: string, password?: string): PatchPromise<Unit> {
    const data: ReqRes.PostAppBackupRestoreReq = {
      password: password || undefined,
      logicalname,
    }
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/backup/restore`, data, readTimeout: 60000 })
  }

  async patchAppConfigRaw (app: AppInstalledPreview, config: object, dryRun = false): PatchPromise<{ breakages: DependentBreakage[] }> {
    const data: ReqRes.PatchAppConfigReq = {
      config,
    }
    return this.authRequest({ method: Method.PATCH, url: `/apps/${app.id}/config${dryRunParam(dryRun, true)}`, data, readTimeout: 60000 })
  }

  async postConfigureDependencyRaw (dependencyId: string, dependentId: string, dryRun?: boolean): PatchPromise<{ config: object, breakages: DependentBreakage[] }> {
    return this.authRequest({ method: Method.POST, url: `/apps/${dependencyId}/autoconfig/${dependentId}${dryRunParam(dryRun, true)}`, readTimeout: 60000 })
  }

  async patchServerConfigRaw (attr: string, value: any): PatchPromise<Unit> {
    const data: ReqRes.PatchServerConfigReq = {
      value,
    }
    return this.authRequest({ method: Method.PATCH, url: `/${attr}`, data, readTimeout: 60000 })
  }

  async wipeAppDataRaw (app: AppInstalledPreview): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${app.id}/wipe`, readTimeout: 60000 })
  }

  async toggleAppLANRaw (appId: string, toggle: 'enable' | 'disable'): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/lan/${toggle}` })
  }

  async addSSHKeyRaw (sshKey: string): PatchPromise<Unit> {
    const data: ReqRes.PostAddSSHKeyReq = {
      sshKey,
    }
    return this.authRequest({ method: Method.POST, url: `/sshKeys`, data })
  }

  async addWifiRaw (ssid: string, password: string, country: string, connect: boolean): PatchPromise<Unit> {
    const data: ReqRes.PostAddWifiReq = {
      ssid,
      password,
      country,
      skipConnect: !connect,
    }
    return this.authRequest({ method: Method.POST, url: `/wifi`, data })
  }

  async connectWifiRaw (ssid: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: encodeURI(`/wifi/${ssid}`) })
  }

  async deleteWifiRaw (ssid: string): PatchPromise<Unit> {
    return this.authRequest({ method: Method.DELETE, url: encodeURI(`/wifi/${ssid}`) })
  }

  async deleteSSHKeyRaw (fingerprint: SSHFingerprint): PatchPromise<Unit> {
    return this.authRequest({ method: Method.DELETE, url: `/sshKeys/${fingerprint.hash}` })
  }

  async restartServerRaw (): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: '/restart', readTimeout: 60000 })
  }

  async shutdownServerRaw (): PatchPromise<Unit> {
    return this.authRequest({ method: Method.POST, url: '/shutdown', readTimeout: 60000 })
  }

  async serviceActionRaw (appId: string, s: ServiceAction): PatchPromise<ReqRes.ServiceActionResponse> {
    const data: ReqRes.ServiceActionRequest = {
      jsonrpc: '2.0',
      id: uuid.v4(),
      method: s.id,
    }
    return this.authRequest({ method: Method.POST, url: `apps/${appId}/actions`, data })
  }

  private async authRequest<T> (opts: HttpOptions, overrides: Partial<{ version: string }> = { }): Promise<T> {
    if (!this.authenticatedRequestsEnabled) throw new Error(`Authenticated requests are not enabled. Do you need to login?`)

    opts.withCredentials = true
    return this.http.serverRequest<T>(opts, overrides).catch((e: HttpError) => {
      console.log(`Got a server error!`, e)
      if (isUnauthorized(e)) this.received401()
      throw e
    })
  }
}

type HttpError = HttpErrorResponse & { error: { code: string, message: string } }

const dryRunParam = (dryRun: boolean, first: boolean) => {
  if (!dryRun) return ''
  return first ? `?dryrun` : `&dryrun`
}

function catchHttpStatusError (error: HttpErrorResponse): Observable<true> {
  if (error.error instanceof ErrorEvent) {
    // A client-side or network error occurred. Handle it accordingly.
    return throwError('Not Connected')
  } else {
    return of(true)
  }
}
