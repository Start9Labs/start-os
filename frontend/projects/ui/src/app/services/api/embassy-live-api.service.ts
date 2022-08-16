import { Injectable } from '@angular/core'
import { HttpService, Log, LogsRes, Method } from '@start9labs/shared'
import { ApiService } from './embassy-api.service'
import { RR } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { ConfigService } from '../config.service'
import { webSocket, WebSocketSubjectConfig } from 'rxjs/webSocket'
import { Observable } from 'rxjs'

@Injectable()
export class LiveApiService extends ApiService {
  constructor(
    private readonly http: HttpService,
    private readonly config: ConfigService,
  ) {
    super()
    ; (window as any).rpcClient = this
  }

  openLogsWebsocket$(config: WebSocketSubjectConfig<Log>): Observable<Log> {
    return webSocket(config)
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
    return this.http.rpcRequest({ method: 'db.revisions', params: { since } })
  }

  async getDump(): Promise<RR.GetDumpRes> {
    return this.http.rpcRequest({ method: 'db.dump', params: {} })
  }

  async setDbValueRaw(params: RR.SetDBValueReq): Promise<RR.SetDBValueRes> {
    return this.http.rpcRequest({ method: 'db.put.ui', params })
  }

  // auth

  async login(params: RR.LoginReq): Promise<RR.loginRes> {
    return this.http.rpcRequest({ method: 'auth.login', params })
  }

  async logout(params: RR.LogoutReq): Promise<RR.LogoutRes> {
    return this.http.rpcRequest({ method: 'auth.logout', params })
  }

  async getSessions(params: RR.GetSessionsReq): Promise<RR.GetSessionsRes> {
    return this.http.rpcRequest({ method: 'auth.session.list', params })
  }

  async killSessions(params: RR.KillSessionsReq): Promise<RR.KillSessionsRes> {
    return this.http.rpcRequest({ method: 'auth.session.kill', params })
  }

  // server

  async getServerLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    return this.http.rpcRequest({ method: 'server.logs', params })
  }

  async getKernelLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    return this.http.rpcRequest({ method: 'server.kernel-logs', params })
  }

  async followServerLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.http.rpcRequest({ method: 'server.logs.follow', params })
  }

  async followKernelLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.http.rpcRequest({ method: 'server.kernel-logs.follow', params })
  }

  async getServerMetrics(
    params: RR.GetServerMetricsReq,
  ): Promise<RR.GetServerMetricsRes> {
    return this.http.rpcRequest({ method: 'server.metrics', params })
  }

  async updateServerRaw(
    params: RR.UpdateServerReq,
  ): Promise<RR.UpdateServerRes> {
    return this.http.rpcRequest({ method: 'server.update', params })
  }

  async restartServer(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes> {
    return this.http.rpcRequest({ method: 'server.restart', params })
  }

  async shutdownServer(
    params: RR.ShutdownServerReq,
  ): Promise<RR.ShutdownServerRes> {
    return this.http.rpcRequest({ method: 'server.shutdown', params })
  }

  async systemRebuild(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes> {
    return this.http.rpcRequest({ method: 'server.rebuild', params })
  }

  async repairDisk(params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    return this.http.rpcRequest({ method: 'disk.repair', params })
  }

  // marketplace URLs

  async marketplaceProxy<T>(path: string, qp: {}, url: string): Promise<T> {
    Object.assign(qp, { arch: this.config.targetArch })
    const fullURL = `${url}${path}?${new URLSearchParams(qp).toString()}`
    return this.http.rpcRequest({
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
    return this.http.rpcRequest({ method: 'notification.list', params })
  }

  async deleteNotification(
    params: RR.DeleteNotificationReq,
  ): Promise<RR.DeleteNotificationRes> {
    return this.http.rpcRequest({ method: 'notification.delete', params })
  }

  async deleteAllNotifications(
    params: RR.DeleteAllNotificationsReq,
  ): Promise<RR.DeleteAllNotificationsRes> {
    return this.http.rpcRequest({
      method: 'notification.delete-before',
      params,
    })
  }

  // wifi

  async getWifi(
    params: RR.GetWifiReq,
    timeout?: number,
  ): Promise<RR.GetWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.get', params, timeout })
  }

  async setWifiCountry(
    params: RR.SetWifiCountryReq,
  ): Promise<RR.SetWifiCountryRes> {
    return this.http.rpcRequest({ method: 'wifi.country.set', params })
  }

  async addWifi(params: RR.AddWifiReq): Promise<RR.AddWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.add', params })
  }

  async connectWifi(params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.connect', params })
  }

  async deleteWifi(params: RR.DeleteWifiReq): Promise<RR.DeleteWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.delete', params })
  }

  // ssh

  async getSshKeys(params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes> {
    return this.http.rpcRequest({ method: 'ssh.list', params })
  }

  async addSshKey(params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes> {
    return this.http.rpcRequest({ method: 'ssh.add', params })
  }

  async deleteSshKey(params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes> {
    return this.http.rpcRequest({ method: 'ssh.delete', params })
  }

  // backup

  async getBackupTargets(
    params: RR.GetBackupTargetsReq,
  ): Promise<RR.GetBackupTargetsRes> {
    return this.http.rpcRequest({ method: 'backup.target.list', params })
  }

  async addBackupTarget(
    params: RR.AddBackupTargetReq,
  ): Promise<RR.AddBackupTargetRes> {
    params.path = params.path.replace('/\\/g', '/')
    return this.http.rpcRequest({ method: 'backup.target.cifs.add', params })
  }

  async updateBackupTarget(
    params: RR.UpdateBackupTargetReq,
  ): Promise<RR.UpdateBackupTargetRes> {
    return this.http.rpcRequest({ method: 'backup.target.cifs.update', params })
  }

  async removeBackupTarget(
    params: RR.RemoveBackupTargetReq,
  ): Promise<RR.RemoveBackupTargetRes> {
    return this.http.rpcRequest({ method: 'backup.target.cifs.remove', params })
  }

  async getBackupInfo(
    params: RR.GetBackupInfoReq,
  ): Promise<RR.GetBackupInfoRes> {
    return this.http.rpcRequest({ method: 'backup.target.info', params })
  }

  async createBackupRaw(
    params: RR.CreateBackupReq,
  ): Promise<RR.CreateBackupRes> {
    return this.http.rpcRequest({ method: 'backup.create', params })
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
    return this.http.rpcRequest({ method: 'package.logs', params })
  }

  async followPackageLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.http.rpcRequest({ method: 'package.logs.follow', params })
  }

  async getPkgMetrics(
    params: RR.GetPackageMetricsReq,
  ): Promise<RR.GetPackageMetricsRes> {
    return this.http.rpcRequest({ method: 'package.metrics', params })
  }

  async installPackageRaw(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes> {
    return this.http.rpcRequest({ method: 'package.install', params })
  }

  async dryUpdatePackage(
    params: RR.DryUpdatePackageReq,
  ): Promise<RR.DryUpdatePackageRes> {
    return this.http.rpcRequest({ method: 'package.update.dry', params })
  }

  async getPackageConfig(
    params: RR.GetPackageConfigReq,
  ): Promise<RR.GetPackageConfigRes> {
    return this.http.rpcRequest({ method: 'package.config.get', params })
  }

  async drySetPackageConfig(
    params: RR.DrySetPackageConfigReq,
  ): Promise<RR.DrySetPackageConfigRes> {
    return this.http.rpcRequest({ method: 'package.config.set.dry', params })
  }

  async setPackageConfigRaw(
    params: RR.SetPackageConfigReq,
  ): Promise<RR.SetPackageConfigRes> {
    return this.http.rpcRequest({ method: 'package.config.set', params })
  }

  async restorePackagesRaw(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes> {
    return this.http.rpcRequest({ method: 'package.backup.restore', params })
  }

  async executePackageAction(
    params: RR.ExecutePackageActionReq,
  ): Promise<RR.ExecutePackageActionRes> {
    return this.http.rpcRequest({ method: 'package.action', params })
  }

  async startPackageRaw(
    params: RR.StartPackageReq,
  ): Promise<RR.StartPackageRes> {
    return this.http.rpcRequest({ method: 'package.start', params })
  }

  async restartPackageRaw(
    params: RR.RestartPackageReq,
  ): Promise<RR.RestartPackageRes> {
    return this.http.rpcRequest({ method: 'package.restart', params })
  }

  async stopPackageRaw(params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    return this.http.rpcRequest({ method: 'package.stop', params })
  }

  async deleteRecoveredPackageRaw(
    params: RR.DeleteRecoveredPackageReq,
  ): Promise<RR.DeleteRecoveredPackageRes> {
    return this.http.rpcRequest({ method: 'package.delete-recovered', params })
  }

  async uninstallPackageRaw(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes> {
    return this.http.rpcRequest({ method: 'package.uninstall', params })
  }

  async dryConfigureDependency(
    params: RR.DryConfigureDependencyReq,
  ): Promise<RR.DryConfigureDependencyRes> {
    return this.http.rpcRequest({
      method: 'package.dependency.configure.dry',
      params,
    })
  }

  async sideloadPackage(
    params: RR.SideloadPackageReq,
  ): Promise<RR.SideloadPacakgeRes> {
    return this.http.rpcRequest({
      method: 'package.sideload',
      params,
    })
  }
}
