import { Injectable } from '@angular/core'
import { HttpService, Method, HttpOptions } from '../http.service'
import { AppModel, AppStatus } from '../../models/app-model'
import { AppAvailablePreview, AppAvailableFull, AppInstalledFull, AppInstalledPreview, DependentBreakage, AppAvailableVersionSpecificInfo } from '../../models/app-types'
import { S9Notification, SSHFingerprint, ServerModel, DiskInfo } from '../../models/server-model'
import { ApiService, ReqRes  } from './api.service'
import { ApiServer, Unit } from './api-types'
import { HttpErrorResponse } from '@angular/common/http'
import { isUnauthorized } from 'src/app/util/web.util'
import { Replace } from 'src/app/util/types.util'
import { AppMetrics, parseMetricsPermissive } from 'src/app/util/metrics.util'
import { modulateTime } from 'src/app/util/misc.util'

@Injectable()
export class LiveApiService extends ApiService {
  constructor (
    private readonly http: HttpService,
    // TODO remove app + server model from here. updates to state should be done in a separate class wrapping ApiService + App/ServerModel
    private readonly appModel: AppModel,
    private readonly serverModel: ServerModel,
  ) { super() }

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
    return this.authRequest<ReqRes.GetServerRes>({ method: Method.GET, url: '/', readTimeout: timeout })
  }

  async getVersionLatest (): Promise<ReqRes.GetVersionLatestRes> {
    return this.authRequest<ReqRes.GetVersionLatestRes>({ method: Method.GET, url: '/versionLatest' }, { version: '' })
  }

  async getServerMetrics (): Promise<ReqRes.GetServerMetricsRes> {
    return this.authRequest<ReqRes.GetServerMetricsRes>({ method: Method.GET, url: `/metrics` })
  }

  async getNotifications (page: number, perPage: number): Promise<S9Notification[]> {
    const params: ReqRes.GetNotificationsReq = {
      page: String(page),
      perPage: String(perPage),
    }
    return this.authRequest<ReqRes.GetNotificationsRes>({ method: Method.GET, url: `/notifications`, params })
  }

  async deleteNotification (id: string): Promise<Unit> {
    return this.authRequest({ method: Method.DELETE, url: `/notifications/${id}` })
  }

  async getExternalDisks (): Promise<DiskInfo[]> {
    return this.authRequest<ReqRes.GetExternalDisksRes>({ method: Method.GET, url: `/disks` })
  }

  async updateAgent (version: string): Promise<Unit> {
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
    return this.authRequest<ReqRes.GetAppsAvailableRes>({ method: Method.GET, url: '/apps/store' })
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

  async getInstalledApps (): Promise<AppInstalledPreview[]> {
    return this.authRequest<ReqRes.GetAppsInstalledRes>({ method: Method.GET, url: `/apps/installed` })
  }

  async getAppConfig ( appId: string): Promise<ReqRes.GetAppConfigRes> {
    return this.authRequest<ReqRes.GetAppConfigRes>({ method: Method.GET, url: `/apps/${appId}/config` })
  }

  async getAppLogs (appId: string, params: ReqRes.GetAppLogsReq = { }): Promise<string[]> {
    return this.authRequest<ReqRes.GetAppLogsRes>( { method: Method.GET, url: `/apps/${appId}/logs`, params: params as any })
  }

  async getAppMetrics (appId: string): Promise<AppMetrics> {
    return this.authRequest<ReqRes.GetAppMetricsRes | string>( { method: Method.GET, url: `/apps/${appId}/metrics` })
      .then(parseMetricsPermissive)
  }

  async installApp (appId: string, version: string, dryRun: boolean = false): Promise<AppInstalledFull & { breakages: DependentBreakage[] }> {
    const data: ReqRes.PostInstallAppReq = {
      version,
    }
    return this.authRequest<ReqRes.PostInstallAppRes>({ method: Method.POST, url: `/apps/${appId}/install${dryRunParam(dryRun, true)}`, data })
      .then(res => ({ ...res, hasFetchedFull: false }))
  }

  async uninstallApp (appId: string, dryRun: boolean = false): Promise<{ breakages: DependentBreakage[] }> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/uninstall${dryRunParam(dryRun, true)}`, readTimeout: 60000 })
  }

  async startApp (appId: string): Promise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/start`, readTimeout: 60000 })
      .then(() => this.appModel.update({ id: appId, status: AppStatus.RUNNING }))
      .then(() => ({ }))
  }

  async stopApp (appId: string, dryRun: boolean = false): Promise<{ breakages: DependentBreakage[] }> {
    const res = await this.authRequest<{ breakages: DependentBreakage[] }>({ method: Method.POST, url: `/apps/${appId}/stop${dryRunParam(dryRun, true)}`, readTimeout: 60000 })
    if (!dryRun) this.appModel.update({ id: appId, status: AppStatus.STOPPING }, modulateTime(new Date(), 5, 'seconds'))
    return res
  }

  async restartApp (appId: string): Promise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${appId}/restart`, readTimeout: 60000 })
      .then(() => ({ } as any))
  }

  async createAppBackup (appId: string, logicalname: string, password?: string): Promise<Unit> {
    const data: ReqRes.PostAppBackupCreateReq = {
      password: password || undefined,
      logicalname,
    }
    return this.authRequest<ReqRes.PostAppBackupCreateRes>({ method: Method.POST, url: `/apps/${appId}/backup`, data, readTimeout: 60000 })
      .then(() => this.appModel.update({ id: appId, status: AppStatus.CREATING_BACKUP }))
      .then(() => ({ }))
  }

  async stopAppBackup (appId: string): Promise<Unit> {
    return this.authRequest<ReqRes.PostAppBackupStopRes>({ method: Method.POST, url: `/apps/${appId}/backup/stop`, readTimeout: 60000 })
      .then(() => this.appModel.update({ id: appId, status: AppStatus.STOPPED }))
      .then(() => ({ }))
  }

  async restoreAppBackup (appId: string, logicalname: string, password?: string): Promise<Unit> {
    const data: ReqRes.PostAppBackupRestoreReq = {
      password: password || undefined,
      logicalname,
    }
    return this.authRequest<ReqRes.PostAppBackupRestoreRes>({ method: Method.POST, url: `/apps/${appId}/backup/restore`, data, readTimeout: 60000 })
      .then(() => this.appModel.update({ id: appId, status: AppStatus.RESTORING_BACKUP }))
      .then(() => ({ }))
  }

  async patchAppConfig (app: AppInstalledPreview, config: object, dryRun = false): Promise<{ breakages: DependentBreakage[] }> {
    const data: ReqRes.PatchAppConfigReq = {
      config,
    }
    return this.authRequest({ method: Method.PATCH, url: `/apps/${app.id}/config${dryRunParam(dryRun, true)}`, data, readTimeout: 60000 })
  }

  async postConfigureDependency (dependencyId: string, dependentId: string, dryRun?: boolean): Promise<{ config: object, breakages: DependentBreakage[] }> {
    return this.authRequest({ method: Method.POST, url: `/apps/${dependencyId}/autoconfig/${dependentId}${dryRunParam(dryRun, true)}`, readTimeout: 60000 })
  }

  async patchServerConfig (attr: string, value: any): Promise<Unit> {
    const data: ReqRes.PatchServerConfigReq = {
      value,
    }
    return this.authRequest({ method: Method.PATCH, url: `/${attr}`, data, readTimeout: 60000 })
      .then(() => this.serverModel.update({ [attr]: value }))
      .then(() => ({ }))
  }

  async wipeAppData (app: AppInstalledPreview): Promise<Unit> {
    return this.authRequest({ method: Method.POST, url: `/apps/${app.id}/wipe`, readTimeout: 60000 }).then((res) => {
      this.appModel.update({ id: app.id, status: AppStatus.NEEDS_CONFIG })
      return res
    })
  }

  async addSSHKey (sshKey: string): Promise<Unit> {
    const data: ReqRes.PostAddSSHKeyReq = {
      sshKey,
    }
    const fingerprint = await this.authRequest<ReqRes.PostAddSSHKeyRes>({ method: Method.POST, url: `/sshKeys`, data })
    this.serverModel.update({ ssh: [...this.serverModel.peek().ssh, fingerprint] })
    return { }
  }

  async addWifi (ssid: string, password: string, country: string, connect: boolean): Promise<Unit> {
    const data: ReqRes.PostAddWifiReq = {
      ssid,
      password,
      country,
      skipConnect: !connect,
    }
    return this.authRequest({ method: Method.POST, url: `/wifi`, data })
  }

  async connectWifi (ssid: string): Promise<Unit> {
    return this.authRequest({ method: Method.POST, url: encodeURI(`/wifi/${ssid}`) })
  }

  async deleteWifi (ssid: string): Promise<Unit> {
    return this.authRequest({ method: Method.DELETE, url: encodeURI(`/wifi/${ssid}`) })
  }

  async deleteSSHKey (fingerprint: SSHFingerprint): Promise<Unit> {
    await this.authRequest({ method: Method.DELETE, url: `/sshKeys/${fingerprint.hash}` })
    const ssh = this.serverModel.peek().ssh
    this.serverModel.update({ ssh: ssh.filter(s => s !== fingerprint) })
    return { }
  }

  async restartServer (): Promise<Unit> {
    return this.authRequest({ method: Method.POST, url: '/restart', readTimeout: 60000 })
  }

  async shutdownServer (): Promise<Unit> {
    return this.authRequest({ method: Method.POST, url: '/shutdown', readTimeout: 60000 })
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