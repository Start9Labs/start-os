import { Inject, Injectable } from '@angular/core'
import {
  HttpService,
  Log,
  Method,
  RPCError,
  RPCOptions,
} from '@start9labs/shared'
import { ApiService } from './embassy-api.service'
import { RR } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { ConfigService } from '../config.service'
import { webSocket, WebSocketSubjectConfig } from 'rxjs/webSocket'
import { Observable, timeout } from 'rxjs'
import { AuthService } from '../auth.service'
import { DOCUMENT } from '@angular/common'
import { DataModel } from '../patch-db/data-model'
import { Update } from 'patch-db-client'

@Injectable()
export class LiveApiService extends ApiService {
  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly http: HttpService,
    private readonly config: ConfigService,
    private readonly auth: AuthService,
  ) {
    super()
    ; (window as any).rpcClient = this
  }

  async getStatic(url: string): Promise<string> {
    return this.http.httpRequest({
      method: Method.GET,
      url,
      responseType: 'text',
    })
  }

  async uploadPackage(guid: string, body: ArrayBuffer): Promise<string> {
    return this.http.httpRequest({
      method: Method.POST,
      body,
      url: `/rest/rpc/${guid}`,
      responseType: 'text',
    })
  }

  // db

  async getRevisions(since: number): Promise<RR.GetRevisionsRes> {
    return this.rpcRequest({ method: 'db.revisions', params: { since } })
  }

  async getDump(): Promise<RR.GetDumpRes> {
    return this.rpcRequest({ method: 'db.dump', params: {} })
  }

  async setDbValueRaw(params: RR.SetDBValueReq): Promise<RR.SetDBValueRes> {
    return this.rpcRequest({ method: 'db.put.ui', params })
  }

  // auth

  async login(params: RR.LoginReq): Promise<RR.loginRes> {
    return this.rpcRequest({ method: 'auth.login', params })
  }

  async logout(params: RR.LogoutReq): Promise<RR.LogoutRes> {
    return this.rpcRequest({ method: 'auth.logout', params })
  }

  async getSessions(params: RR.GetSessionsReq): Promise<RR.GetSessionsRes> {
    return this.rpcRequest({ method: 'auth.session.list', params })
  }

  async killSessions(params: RR.KillSessionsReq): Promise<RR.KillSessionsRes> {
    return this.rpcRequest({ method: 'auth.session.kill', params })
  }

  // server

  async echo(params: RR.EchoReq): Promise<RR.EchoRes> {
    return this.rpcRequest({ method: 'echo', params })
  }

  openPatchWebsocket$(): Observable<Update<DataModel>> {
    const config: WebSocketSubjectConfig<Update<DataModel>> = {
      url: `/db`,
      closeObserver: {
        next: val => {
          if (val.reason === 'UNAUTHORIZED') this.auth.setUnverified()
        },
      },
    }

    return this.openWebsocket(config).pipe(timeout({ first: 21000 }))
  }

  openLogsWebsocket$(config: WebSocketSubjectConfig<Log>): Observable<Log> {
    return this.openWebsocket(config)
  }

  async getServerLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    return this.rpcRequest({ method: 'server.logs', params })
  }

  async getKernelLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    return this.rpcRequest({ method: 'server.kernel-logs', params })
  }

  async followServerLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.rpcRequest({ method: 'server.logs.follow', params })
  }

  async followKernelLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.rpcRequest({ method: 'server.kernel-logs.follow', params })
  }

  async getServerMetrics(
    params: RR.GetServerMetricsReq,
  ): Promise<RR.GetServerMetricsRes> {
    return this.rpcRequest({ method: 'server.metrics', params })
  }

  async updateServerRaw(
    params: RR.UpdateServerReq,
  ): Promise<RR.UpdateServerRes> {
    return this.rpcRequest({ method: 'server.update', params })
  }

  async restartServer(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes> {
    return this.rpcRequest({ method: 'server.restart', params })
  }

  async shutdownServer(
    params: RR.ShutdownServerReq,
  ): Promise<RR.ShutdownServerRes> {
    return this.rpcRequest({ method: 'server.shutdown', params })
  }

  async systemRebuild(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes> {
    return this.rpcRequest({ method: 'server.rebuild', params })
  }

  async repairDisk(params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    return this.rpcRequest({ method: 'disk.repair', params })
  }

  // marketplace URLs

  async marketplaceProxy<T>(path: string, qp: {}, url: string): Promise<T> {
    Object.assign(qp, { arch: this.config.targetArch })
    const fullURL = `${url}${path}?${new URLSearchParams(qp).toString()}`
    return this.rpcRequest({
      method: 'marketplace.get',
      params: { url: fullURL },
    })
  }

  async getEos(
    params: RR.GetMarketplaceEOSReq,
  ): Promise<RR.GetMarketplaceEOSRes> {
    return this.marketplaceProxy(
      '/eos/v0/latest',
      params,
      this.config.marketplace.url,
    )
  }

  // notification

  async getNotificationsRaw(
    params: RR.GetNotificationsReq,
  ): Promise<RR.GetNotificationsRes> {
    return this.rpcRequest({ method: 'notification.list', params })
  }

  async deleteNotification(
    params: RR.DeleteNotificationReq,
  ): Promise<RR.DeleteNotificationRes> {
    return this.rpcRequest({ method: 'notification.delete', params })
  }

  async deleteAllNotifications(
    params: RR.DeleteAllNotificationsReq,
  ): Promise<RR.DeleteAllNotificationsRes> {
    return this.rpcRequest({
      method: 'notification.delete-before',
      params,
    })
  }

  // wifi

  async getWifi(
    params: RR.GetWifiReq,
    timeout?: number,
  ): Promise<RR.GetWifiRes> {
    return this.rpcRequest({ method: 'wifi.get', params, timeout })
  }

  async setWifiCountry(
    params: RR.SetWifiCountryReq,
  ): Promise<RR.SetWifiCountryRes> {
    return this.rpcRequest({ method: 'wifi.country.set', params })
  }

  async addWifi(params: RR.AddWifiReq): Promise<RR.AddWifiRes> {
    return this.rpcRequest({ method: 'wifi.add', params })
  }

  async connectWifi(params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes> {
    return this.rpcRequest({ method: 'wifi.connect', params })
  }

  async deleteWifi(params: RR.DeleteWifiReq): Promise<RR.DeleteWifiRes> {
    return this.rpcRequest({ method: 'wifi.delete', params })
  }

  // ssh

  async getSshKeys(params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes> {
    return this.rpcRequest({ method: 'ssh.list', params })
  }

  async addSshKey(params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes> {
    return this.rpcRequest({ method: 'ssh.add', params })
  }

  async deleteSshKey(params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes> {
    return this.rpcRequest({ method: 'ssh.delete', params })
  }

  // backup

  async getBackupTargets(
    params: RR.GetBackupTargetsReq,
  ): Promise<RR.GetBackupTargetsRes> {
    return this.rpcRequest({ method: 'backup.target.list', params })
  }

  async addBackupTarget(
    params: RR.AddBackupTargetReq,
  ): Promise<RR.AddBackupTargetRes> {
    params.path = params.path.replace('/\\/g', '/')
    return this.rpcRequest({ method: 'backup.target.cifs.add', params })
  }

  async updateBackupTarget(
    params: RR.UpdateBackupTargetReq,
  ): Promise<RR.UpdateBackupTargetRes> {
    return this.rpcRequest({ method: 'backup.target.cifs.update', params })
  }

  async removeBackupTarget(
    params: RR.RemoveBackupTargetReq,
  ): Promise<RR.RemoveBackupTargetRes> {
    return this.rpcRequest({ method: 'backup.target.cifs.remove', params })
  }

  async getBackupInfo(
    params: RR.GetBackupInfoReq,
  ): Promise<RR.GetBackupInfoRes> {
    return this.rpcRequest({ method: 'backup.target.info', params })
  }

  async createBackupRaw(
    params: RR.CreateBackupReq,
  ): Promise<RR.CreateBackupRes> {
    return this.rpcRequest({ method: 'backup.create', params })
  }

  // package

  async getPackageProperties(
    params: RR.GetPackagePropertiesReq,
  ): Promise<RR.GetPackagePropertiesRes<2>['data']> {
    return this.http
      .rpcRequest({ method: 'package.properties', params })
      .then(parsePropertiesPermissive)
  }

  async getPackageLogs(
    params: RR.GetPackageLogsReq,
  ): Promise<RR.GetPackageLogsRes> {
    return this.rpcRequest({ method: 'package.logs', params })
  }

  async followPackageLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.rpcRequest({ method: 'package.logs.follow', params })
  }

  async getPkgMetrics(
    params: RR.GetPackageMetricsReq,
  ): Promise<RR.GetPackageMetricsRes> {
    return this.rpcRequest({ method: 'package.metrics', params })
  }

  async installPackageRaw(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes> {
    return this.rpcRequest({ method: 'package.install', params })
  }

  async dryUpdatePackage(
    params: RR.DryUpdatePackageReq,
  ): Promise<RR.DryUpdatePackageRes> {
    return this.rpcRequest({ method: 'package.update.dry', params })
  }

  async getPackageConfig(
    params: RR.GetPackageConfigReq,
  ): Promise<RR.GetPackageConfigRes> {
    return this.rpcRequest({ method: 'package.config.get', params })
  }

  async drySetPackageConfig(
    params: RR.DrySetPackageConfigReq,
  ): Promise<RR.DrySetPackageConfigRes> {
    return this.rpcRequest({ method: 'package.config.set.dry', params })
  }

  async setPackageConfigRaw(
    params: RR.SetPackageConfigReq,
  ): Promise<RR.SetPackageConfigRes> {
    return this.rpcRequest({ method: 'package.config.set', params })
  }

  async restorePackagesRaw(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes> {
    return this.rpcRequest({ method: 'package.backup.restore', params })
  }

  async executePackageAction(
    params: RR.ExecutePackageActionReq,
  ): Promise<RR.ExecutePackageActionRes> {
    return this.rpcRequest({ method: 'package.action', params })
  }

  async startPackageRaw(
    params: RR.StartPackageReq,
  ): Promise<RR.StartPackageRes> {
    return this.rpcRequest({ method: 'package.start', params })
  }

  async restartPackageRaw(
    params: RR.RestartPackageReq,
  ): Promise<RR.RestartPackageRes> {
    return this.rpcRequest({ method: 'package.restart', params })
  }

  async stopPackageRaw(params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    return this.rpcRequest({ method: 'package.stop', params })
  }

  async deleteRecoveredPackageRaw(
    params: RR.DeleteRecoveredPackageReq,
  ): Promise<RR.DeleteRecoveredPackageRes> {
    return this.rpcRequest({ method: 'package.delete-recovered', params })
  }

  async uninstallPackageRaw(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes> {
    return this.rpcRequest({ method: 'package.uninstall', params })
  }

  async dryConfigureDependency(
    params: RR.DryConfigureDependencyReq,
  ): Promise<RR.DryConfigureDependencyRes> {
    return this.rpcRequest({
      method: 'package.dependency.configure.dry',
      params,
    })
  }

  async sideloadPackage(
    params: RR.SideloadPackageReq,
  ): Promise<RR.SideloadPacakgeRes> {
    return this.rpcRequest({
      method: 'package.sideload',
      params,
    })
  }

  private openWebsocket<T>(config: WebSocketSubjectConfig<T>): Observable<T> {
    const { location } = this.document.defaultView!
    const protocol = location.protocol === 'http:' ? 'ws' : 'wss'
    const host = location.host

    config.url = `${protocol}://${host}/ws${config.url}`

    return webSocket(config)
  }

  private async rpcRequest<T>(options: RPCOptions): Promise<T> {
    return this.http.rpcRequest<T>(options).catch(e => {
      if ((e as RPCError).error.code === 34) {
        console.error('Unauthenticated, logging out')
        this.auth.setUnverified()
      }
      throw e
    })
  }
}
