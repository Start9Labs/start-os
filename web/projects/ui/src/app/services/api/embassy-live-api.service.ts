import { Inject, Injectable } from '@angular/core'
import {
  HttpOptions,
  HttpService,
  isRpcError,
  Method,
  RpcError,
  RPCOptions,
} from '@start9labs/shared'
import { PATCH_CACHE } from 'src/app/services/patch-db/patch-db-source'
import { ApiService } from './embassy-api.service'
import { RR } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { ConfigService } from '../config.service'
import { webSocket } from 'rxjs/webSocket'
import { Observable, filter, firstValueFrom } from 'rxjs'
import { AuthService } from '../auth.service'
import { DOCUMENT } from '@angular/common'
import { DataModel } from '../patch-db/data-model'
import { Dump, pathFromArray } from 'patch-db-client'
import { T } from '@start9labs/start-sdk'
import { MarketplacePkg } from '@start9labs/marketplace'
import { blake3 } from '@noble/hashes/blake3'

@Injectable()
export class LiveApiService extends ApiService {
  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly http: HttpService,
    private readonly config: ConfigService,
    private readonly auth: AuthService,
    @Inject(PATCH_CACHE) private readonly cache$: Observable<Dump<DataModel>>,
  ) {
    super()
    ; (window as any).rpcClient = this
  }

  // for sideloading packages

  async uploadPackage(guid: string, body: Blob): Promise<string> {
    return this.httpRequest({
      method: Method.POST,
      body,
      url: `/rest/rpc/${guid}`,
      responseType: 'text',
    })
  }

  async uploadFile(body: Blob): Promise<string> {
    return this.httpRequest({
      method: Method.POST,
      body,
      url: `/rest/upload`,
      responseType: 'text',
    })
  }

  // for getting static files: ex. instructions, licenses

  async getStaticProxy(pkg: MarketplacePkg, path?: string): Promise<string> {
    const encodedUrl = encodeURIComponent(pkg.s9pk.url)
    const url = path
      ? // returns the file at <path> within the s9pk at encoded url. optionally takes query params to verify the s9pk
        `/s9pk/proxy/${encodedUrl}/${path}.md
          ?rootSighash=${pkg.s9pk.commitment.rootSighash}
          &rootMaxsize=${pkg.s9pk.commitment.rootMaxsize}`
      : // returns the full s9pk at encoded url. optionally takes query params to verify the s9pk
        `/s9pk/proxy/${encodedUrl}
          ?rootSighash=${pkg.s9pk.commitment.rootSighash}
          &rootMaxsize=${pkg.s9pk.commitment.rootMaxsize}`

    return this.httpRequest({
      method: Method.GET,
      url,
      responseType: 'text',
    })
  }

  async getStaticInstalled(id: T.PackageId, path?: string): Promise<string> {
    const url = path
      ? // returns the file at <path> within the s9pk of an installed package
        `/s9pk/installed/${id}.s9pk/${path}.md`
      : // returns the full s9pk of an installed package
        `/s9pk/installed/${id}.s9pk`

    return this.httpRequest({
      method: Method.GET,
      url,
      responseType: 'text',
    })
  }

  // websocket

  openWebsocket$<T>(
    guid: string,
    config: RR.WebsocketConfig<T>,
  ): Observable<T> {
    const { location } = this.document.defaultView!
    const protocol = location.protocol === 'http:' ? 'ws' : 'wss'
    const host = location.host

    return webSocket({
      url: `${protocol}://${host}/ws/rpc/${guid}`,
      ...config,
    })
  }

  // state

  async echo(params: RR.EchoReq, url: string): Promise<RR.EchoRes> {
    return this.rpcRequest({ method: 'echo', params }, url)
  }

  async getState(): Promise<RR.ServerState> {
    return this.rpcRequest({ method: 'state', params: {} })
  }

  // db

  async subscribeToPatchDB(
    params: RR.SubscribePatchReq,
  ): Promise<RR.SubscribePatchRes> {
    return this.rpcRequest({ method: 'db.subscribe', params })
  }

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

  async resetPassword(
    params: RR.ResetPasswordReq,
  ): Promise<RR.ResetPasswordRes> {
    return this.rpcRequest({ method: 'auth.reset-password', params })
  }

  // diagnostic

  async diagnosticGetError(): Promise<RR.DiagnosticErrorRes> {
    return this.rpcRequest<RR.DiagnosticErrorRes>({
      method: 'diagnostic.error',
      params: {},
    })
  }

  async diagnosticRestart(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.restart',
      params: {},
    })
  }

  async diagnosticForgetDrive(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.disk.forget',
      params: {},
    })
  }

  async diagnosticRepairDisk(): Promise<void> {
    return this.rpcRequest<void>({
      method: 'diagnostic.disk.repair',
      params: {},
    })
  }

  async diagnosticGetLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    return this.rpcRequest<RR.GetServerLogsRes>({
      method: 'diagnostic.logs',
      params,
    })
  }

  // init

  async initGetProgress(): Promise<RR.InitGetProgressRes> {
    return this.rpcRequest({ method: 'init.subscribe', params: {} })
  }

  async initFollowLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.rpcRequest({ method: 'init.logs.follow', params })
  }

  // server

  async getSystemTime(
    params: RR.GetSystemTimeReq,
  ): Promise<RR.GetSystemTimeRes> {
    return this.rpcRequest({ method: 'server.time', params })
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

  async getTorLogs(params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes> {
    return this.rpcRequest({ method: 'net.tor.logs', params })
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

  async followTorLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    return this.rpcRequest({ method: 'net.tor.logs.follow', params })
  }

  async getServerMetrics(
    params: RR.GetServerMetricsReq,
  ): Promise<RR.GetServerMetricsRes> {
    return this.rpcRequest({ method: 'server.metrics', params })
  }

  async updateServer(url?: string): Promise<RR.UpdateServerRes> {
    const params = {
      registry: url || this.config.marketplace.start9,
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

  async repairDisk(params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    return this.rpcRequest({ method: 'disk.repair', params })
  }

  async resetTor(params: RR.ResetTorReq): Promise<RR.ResetTorRes> {
    return this.rpcRequest({ method: 'net.tor.reset', params })
  }

  // marketplace URLs

  async registryRequest<T>(
    registryUrl: string,
    options: RPCOptions,
  ): Promise<T> {
    return this.rpcRequest({
      ...options,
      method: `registry.${options.method}`,
      params: { registry: registryUrl, ...options.params },
    })
  }

  async checkOSUpdate(qp: RR.CheckOSUpdateReq): Promise<RR.CheckOSUpdateRes> {
    const { serverId } = qp

    return this.registryRequest(this.config.marketplace.start9, {
      method: 'os.version.get',
      params: { serverId },
    })
  }

  async getRegistryInfo(registryUrl: string): Promise<RR.GetRegistryInfoRes> {
    return this.registryRequest(registryUrl, {
      method: 'info',
      params: {},
    })
  }

  async getRegistryPackages<T extends T.GetPackageParams>(
    registryUrl: string,
    params: T,
  ): Promise<RR.GetRegistryMultiPackagesRes<T>> {
    return this.registryRequest(registryUrl, {
      method: 'package.get',
      params,
    })
  }

  async getRegistryPackage<T extends T.GetPackageParams>(
    registryUrl: string,
    params: T,
  ): Promise<RR.GetRegistrySinglePackageRes<T>> {
    return this.registryRequest(registryUrl, {
      method: 'package.get',
      params,
    })
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
    params: RR.FollowPackageLogsReq,
  ): Promise<RR.FollowPackageLogsRes> {
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

  async sideloadPackage(): Promise<RR.SideloadPackageRes> {
    return this.rpcRequest({
      method: 'package.sideload',
      params: {},
    })
  }

  private async rpcRequest<T>(
    options: RPCOptions,
    urlOverride?: string,
  ): Promise<T> {
    const res = await this.http.rpcRequest<T>(options, urlOverride)
    const body = res.body

    if (isRpcError(body)) {
      if (body.error.code === 34) {
        console.error('Unauthenticated, logging out')
        this.auth.setUnverified()
      }
      throw new RpcError(body.error)
    }

    const patchSequence = res.headers.get('x-patch-sequence')
    if (patchSequence)
      await firstValueFrom(
        this.cache$.pipe(filter(({ id }) => id >= Number(patchSequence))),
      )

    return body.result
  }

  private async httpRequest<T>(opts: HttpOptions): Promise<T> {
    const res = await this.http.httpRequest<T>(opts)
    if (res.headers.get('Repr-Digest')) {
      // verify
      const digest = res.headers.get('Repr-Digest')!
      const data = new Uint8Array(res.body as ArrayBuffer)
      // TODO confirm
      if (`blake3=:${blake3(data)}:`.toString() === digest) return res.body
      throw new Error('File digest mismatch.')
    }
    return res.body
  }
}
