import { Inject, Injectable } from '@angular/core'
import {
  decodeBase64,
  HttpOptions,
  HttpService,
  isRpcError,
  Log,
  Method,
  RpcError,
  RPCOptions,
} from '@start9labs/shared'
import { ApiService } from './embassy-api.service'
import { RR } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { ConfigService } from '../config.service'
import { webSocket, WebSocketSubjectConfig } from 'rxjs/webSocket'
import { Observable } from 'rxjs'
import { AuthService } from '../auth.service'
import { DOCUMENT } from '@angular/common'
import { DataModel } from '../patch-db/data-model'
import { PatchDB, pathFromArray, Update } from 'patch-db-client'

@Injectable()
export class LiveApiService extends ApiService {
  readonly eosMarketplaceUrl = 'https://registry.start9.com/'

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly http: HttpService,
    private readonly config: ConfigService,
    private readonly auth: AuthService,
    private readonly patch: PatchDB<DataModel>,
  ) {
    super()
    ; (window as any).rpcClient = this
  }

  async getStatic(url: string): Promise<string> {
    return this.httpRequest({
      method: Method.GET,
      url,
      responseType: 'text',
    })
  }

  async uploadPackage(guid: string, body: ArrayBuffer): Promise<string> {
    return this.httpRequest({
      method: Method.POST,
      body,
      url: `/rest/rpc/${guid}`,
      responseType: 'text',
    })
  }

  // db

  async setDbValue<T>(
    pathArr: Array<string | number>,
    value: T,
  ): Promise<RR.SetDBValueRes> {
    const pointer = pathFromArray(pathArr)
    const params: RR.SetDBValueReq<T> = { pointer, value }
    return this.rpcRequest({ method: 'db.put.ui', params })
  }

  // auth

  async login(params: RR.LoginReq): Promise<RR.loginRes> {
    return this.rpcRequest({ method: 'auth.login', params }, false)
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
    return this.rpcRequest({ method: 'echo', params }, false)
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

    return this.openWebsocket(config)
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

  async updateServer(url?: string): Promise<RR.UpdateServerRes> {
    const params = {
      'marketplace-url': url || this.eosMarketplaceUrl,
    }
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

  async marketplaceProxy<T>(path: string, qp: {}, baseUrl: string): Promise<T> {
    Object.assign(qp, { arch: this.config.targetArch })
    const fullUrl = `${baseUrl}${path}?${new URLSearchParams(qp).toString()}`
    return this.rpcRequest({
      method: 'marketplace.get',
      params: { url: fullUrl },
    })
  }

  async getEos(
    params: RR.GetMarketplaceEOSReq,
  ): Promise<RR.GetMarketplaceEOSRes> {
    return this.marketplaceProxy(
      '/eos/v0/latest',
      params,
      this.eosMarketplaceUrl,
    )
  }

  // notification

  async getNotifications(
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

  async createBackup(params: RR.CreateBackupReq): Promise<RR.CreateBackupRes> {
    return this.rpcRequest({ method: 'backup.create', params })
  }

  // package

  async getPackageProperties(
    params: RR.GetPackagePropertiesReq,
  ): Promise<RR.GetPackagePropertiesRes<2>['data']> {
    return this.rpcRequest({ method: 'package.properties', params }).then(
      parsePropertiesPermissive,
    )
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

  async installPackage(
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

  async setPackageConfig(
    params: RR.SetPackageConfigReq,
  ): Promise<RR.SetPackageConfigRes> {
    return this.rpcRequest({ method: 'package.config.set', params })
  }

  async restorePackages(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes> {
    return this.rpcRequest({ method: 'package.backup.restore', params })
  }

  async executePackageAction(
    params: RR.ExecutePackageActionReq,
  ): Promise<RR.ExecutePackageActionRes> {
    return this.rpcRequest({ method: 'package.action', params })
  }

  async startPackage(params: RR.StartPackageReq): Promise<RR.StartPackageRes> {
    return this.rpcRequest({ method: 'package.start', params })
  }

  async restartPackage(
    params: RR.RestartPackageReq,
  ): Promise<RR.RestartPackageRes> {
    return this.rpcRequest({ method: 'package.restart', params })
  }

  async stopPackage(params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    return this.rpcRequest({ method: 'package.stop', params })
  }

  async uninstallPackage(
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

  private async rpcRequest<T>(
    options: RPCOptions,
    addHeader = true,
  ): Promise<T> {
    if (addHeader) {
      options.headers = {
        'x-patch-sequence': String(this.patch.cache$.value.sequence),
        ...(options.headers || {}),
      }
    }

    const res = await this.http.rpcRequest<T>(options)
    const encodedUpdates = res.headers.get('x-patch-updates')
    const encodedError = res.headers.get('x-patch-error')

    if (encodedUpdates) {
      const decoded = decodeBase64(encodedUpdates)
      const updates: Update<DataModel>[] = JSON.parse(decoded)
      this.patchStream$.next(updates)
    }

    if (encodedError) {
      const error = decodeBase64(encodedError)
      console.error(error)
    }

    const rpcRes = res.body

    if (isRpcError(rpcRes)) {
      if (rpcRes.error.code === 34) {
        console.error('Unauthenticated, logging out')
        this.auth.setUnverified()
      }
      throw new RpcError(rpcRes.error)
    }

    return rpcRes.result
  }

  private async httpRequest<T>(opts: HttpOptions): Promise<T> {
    const res = await this.http.httpRequest<T>(opts)
    return res.body
  }
}
