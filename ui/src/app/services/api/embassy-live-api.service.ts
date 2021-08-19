import { Injectable } from '@angular/core'
import { HttpService, Method } from '../http.service'
import { ApiService  } from './embassy-api.service'
import { RR } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'

@Injectable()
export class LiveApiService extends ApiService {

  constructor (
    private readonly http: HttpService,
  ) { super() }

  async getStatic (url: string): Promise<string> {
    return this.http.httpRequest({
      method: Method.GET,
      url,
      responseType: 'text',
    })
  }

  // db

  async getRevisions (since: number): Promise<RR.GetRevisionsRes> {
    return this.http.rpcRequest({ method: 'db.revisions', params: { since } })
  }

  async getDump (): Promise<RR.GetDumpRes> {
    return this.http.rpcRequest({ method: 'db.dump' })
  }

  async setDbValueRaw (params: RR.SetDBValueReq): Promise<RR.SetDBValueRes> {
    return this.http.rpcRequest({ method: 'db.put.ui', params })
  }

  // auth

  async login (params: RR.LoginReq): Promise<RR.loginRes> {
    return this.http.rpcRequest({ method: 'auth.login', params })
  }

  async logout (params: RR.LogoutReq): Promise<RR.LogoutRes> {
    return this.http.rpcRequest({ method: 'auth.logout', params })
  }

  async getSessions (params: RR.GetSessionsReq): Promise<RR.GetSessionsRes> {
    return this.http.rpcRequest({ method: 'auth.session.list', params })
  }

  async killSessions (params: RR.KillSessionsReq): Promise<RR.KillSessionsRes> {
    return this.http.rpcRequest({ method: 'auth.session.kill', params })
  }

  // server

  async setShareStatsRaw (params: RR.SetShareStatsReq): Promise<RR.SetShareStatsRes> {
    return this.http.rpcRequest( { method: 'server.config.share-stats', params })
  }

  async getServerLogs (params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes> {
    return this.http.rpcRequest( { method: 'server.logs', params })
  }

  async getServerMetrics (params: RR.GetServerMetricsReq): Promise<RR.GetServerMetricsRes> {
    return this.http.rpcRequest({ method: 'server.metrics', params })
  }

  async updateServerRaw (params: RR.UpdateServerReq): Promise<RR.UpdateServerRes> {
    return this.http.rpcRequest({ method: 'server.update', params })
  }

  async restartServer (params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    return this.http.rpcRequest({ method: 'server.restart', params })
  }

  async shutdownServer (params: RR.ShutdownServerReq): Promise<RR.ShutdownServerRes> {
    return this.http.rpcRequest({ method: 'server.shutdown', params })
  }

  // network

  async refreshLan (params: RR.RefreshLanReq): Promise<RR.RefreshLanRes> {
    return this.http.rpcRequest({ method: 'network.lan.refresh', params })
  }

  // marketplace URLs

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    return this.http.httpRequest({
      method: Method.GET,
      url: '/marketplace/eos/latest',
      params,
    })
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise <RR.GetMarketplaceDataRes> {
    return this.http.httpRequest({
      method: Method.GET,
      url: '/marketplace/package/data',
      params,
    })
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise <RR.GetMarketplacePackagesRes> {
    return this.http.httpRequest({
      method: Method.GET,
      url: '/marketplace/package/index',
      params: {
        ...params,
        ids: JSON.stringify(params.ids),
      },
    })
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise <RR.GetReleaseNotesRes> {
    return this.http.httpRequest({
      method: Method.GET,
      url: '/marketplace/package/release-notes',
      params,
    })
  }

  async getLatestVersion (params: RR.GetLatestVersionReq): Promise <RR.GetLatestVersionRes> {
    return this.http.httpRequest({
      method: Method.GET,
      url: '/marketplace/latest-version',
      params,
    })
  }

  // async setPackageMarketplaceRaw (params: RR.SetPackageMarketplaceReq): Promise<RR.SetPackageMarketplaceRes> {
  //   return this.http.rpcRequest({ method: 'marketplace.package.set', params })
  // }

  // password
  // async updatePassword (params: RR.UpdatePasswordReq): Promise<RR.UpdatePasswordRes> {
  //   return this.http.rpcRequest({ method: 'password.set', params })
  // }

  // notification

  async getNotificationsRaw (params: RR.GetNotificationsReq): Promise <RR.GetNotificationsRes> {
    return this.http.rpcRequest({ method: 'notification.list', params })
  }

  async deleteNotification (params: RR.DeleteNotificationReq): Promise <RR.DeleteNotificationRes> {
    return this.http.rpcRequest({ method: 'notification.delete', params })
  }

  async deleteAllNotifications (params: RR.DeleteAllNotificationsReq): Promise <RR.DeleteAllNotificationsRes> {
    return this.http.rpcRequest({ method: 'notification.delete.all', params })
  }

  // wifi

  async getWifi (params: RR.GetWifiReq, timeout?: number): Promise <RR.GetWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.get', params, timeout })
  }

  async setWifiCountry (params: RR.SetWifiCountryReq): Promise <RR.SetWifiCountryRes> {
    return this.http.rpcRequest({ method: 'wifi.country.set', params })
  }

  async addWifi (params: RR.AddWifiReq): Promise <RR.AddWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.add', params })
  }

  async connectWifi (params: RR.ConnectWifiReq): Promise <RR.ConnectWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.connect', params })
  }

  async deleteWifi (params: RR.DeleteWifiReq): Promise <RR.DeleteWifiRes> {
    return this.http.rpcRequest({ method: 'wifi.delete', params })
  }

  // ssh

  async getSshKeys (params: RR.GetSSHKeysReq): Promise <RR.GetSSHKeysRes> {
    return this.http.rpcRequest({ method: 'ssh.get', params })
  }

  async addSshKey (params: RR.AddSSHKeyReq): Promise <RR.AddSSHKeyRes> {
    return this.http.rpcRequest({ method: 'ssh.add', params })
  }

  async deleteSshKey (params: RR.DeleteSSHKeyReq): Promise <RR.DeleteSSHKeyRes> {
    return this.http.rpcRequest({ method: 'ssh.delete', params })
  }

  // backup

  async createBackupRaw (params: RR.CreateBackupReq): Promise <RR.CreateBackupRes> {
    return this.http.rpcRequest({ method: 'backup.create', params })
  }

  async restoreBackupRaw (params: RR.RestoreBackupReq): Promise <RR.RestoreBackupRes> {
    return this.http.rpcRequest({ method: 'backup.restore', params })
  }

  // disk

  getDisks (params: RR.GetDisksReq): Promise <RR.GetDisksRes> {
    return this.http.rpcRequest({ method: 'disk.list', params })
  }

  ejectDisk (params: RR.EjectDisksReq): Promise <RR.EjectDisksRes> {
    return this.http.rpcRequest({ method: 'disk.eject', params })
  }

  // package

  async getPackageProperties (params: RR.GetPackagePropertiesReq): Promise <RR.GetPackagePropertiesRes < any > ['data'] > {
    return this.http.rpcRequest({ method: 'package.properties', params })
      .then(parsePropertiesPermissive)
  }

  async getPackageLogs (params: RR.GetPackageLogsReq): Promise <RR.GetPackageLogsRes> {
    return this.http.rpcRequest( { method: 'package.logs', params })
  }

  async getPkgMetrics (params: RR.GetPackageMetricsReq): Promise <RR.GetPackageMetricsRes> {
    return this.http.rpcRequest({ method: 'package.metrics', params })
  }

  async installPackageRaw (params: RR.InstallPackageReq): Promise <RR.InstallPackageRes> {
    return this.http.rpcRequest({ method: 'package.install', params })
  }

  async dryUpdatePackage (params: RR.DryUpdatePackageReq): Promise <RR.DryUpdatePackageRes> {
    return this.http.rpcRequest({ method: 'package.update.dry', params })
  }

  async getPackageConfig (params: RR.GetPackageConfigReq): Promise <RR.GetPackageConfigRes> {
    return this.http.rpcRequest({ method: 'package.config.get', params })
  }

  async drySetPackageConfig (params: RR.DrySetPackageConfigReq): Promise <RR.DrySetPackageConfigRes> {
    return this.http.rpcRequest({ method: 'package.config.set.dry', params })
  }

  async setPackageConfigRaw (params: RR.SetPackageConfigReq): Promise <RR.SetPackageConfigRes> {
    return this.http.rpcRequest({ method: 'package.config.set', params })
  }

  async restorePackageRaw (params: RR.RestorePackageReq): Promise <RR.RestorePackageRes> {
    return this.http.rpcRequest({ method: 'package.restore', params })
  }

  async executePackageAction (params: RR.ExecutePackageActionReq): Promise <RR.ExecutePackageActionRes> {
    return this.http.rpcRequest({ method: 'package.action', params })
  }

  async startPackageRaw (params: RR.StartPackageReq): Promise <RR.StartPackageRes> {
    return this.http.rpcRequest({ method: 'package.start', params })
  }

  async dryStopPackage (params: RR.DryStopPackageReq): Promise <RR.DryStopPackageRes> {
    return this.http.rpcRequest({ method: 'package.stop.dry', params })
  }

  async stopPackageRaw (params: RR.StopPackageReq): Promise <RR.StopPackageRes> {
    return this.http.rpcRequest({ method: 'package.stop', params })
  }

  async dryRemovePackage (params: RR.DryRemovePackageReq): Promise <RR.DryRemovePackageRes> {
    return this.http.rpcRequest({ method: 'package.remove.dry', params })
  }

  async removePackageRaw (params: RR.RemovePackageReq): Promise <RR.RemovePackageRes> {
    return this.http.rpcRequest({ method: 'package.remove', params })
  }

  async dryConfigureDependency (params: RR.DryConfigureDependencyReq): Promise <RR.DryConfigureDependencyRes> {
    return this.http.rpcRequest({ method: 'package.dependency.configure.dry', params })
  }
}
