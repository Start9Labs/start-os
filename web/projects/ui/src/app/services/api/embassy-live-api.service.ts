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
import { webSocket, WebSocketSubject } from 'rxjs/webSocket'
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
    private readonly auth: AuthService,
    @Inject(PATCH_CACHE) private readonly cache$: Observable<Dump<DataModel>>,
  ) {
    super()

    // @ts-ignore
    this.document.defaultView.rpcClient = this
  }

  // for sideloading packages

  async uploadPackage(guid: string, body: Blob): Promise<void> {
    await this.httpRequest({
      method: Method.POST,
      body,
      url: `/rest/rpc/${guid}`,
    })
  }

  // for getting static files: ex. instructions, licenses

  async getStaticProxy(
    pkg: MarketplacePkg,
    path: 'LICENSE.md' | 'instructions.md',
  ): Promise<string> {
    const encodedUrl = encodeURIComponent(pkg.s9pk.url)

    return this.httpRequest({
      method: Method.GET,
      url: `/s9pk/proxy/${encodedUrl}/${path}`,
      params: {
        rootSighash: pkg.s9pk.commitment.rootSighash,
        rootMaxsize: pkg.s9pk.commitment.rootMaxsize,
      },
      responseType: 'text',
    })
  }

  async getStaticInstalled(
    id: T.PackageId,
    path: 'LICENSE.md' | 'instructions.md',
  ): Promise<string> {
    return this.httpRequest({
      method: Method.GET,
      url: `/s9pk/installed/${id}.s9pk/${path}`,
      responseType: 'text',
    })
  }

  // websocket

  openWebsocket$<T>(
    guid: string,
    config: RR.WebsocketConfig<T> = {},
  ): WebSocketSubject<T> {
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

  async initFollowProgress(): Promise<RR.InitFollowProgressRes> {
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

  async followServerMetrics(
    params: RR.FollowServerMetricsReq,
  ): Promise<RR.FollowServerMetricsRes> {
    return this.rpcRequest({ method: 'server.metrics.follow', params })
  }

  async updateServer(params: RR.UpdateServerReq): Promise<RR.UpdateServerRes> {
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

  // async setOsOutboundProxy(
  //   params: RR.SetOsOutboundProxyReq,
  // ): Promise<RR.SetOsOutboundProxyRes> {
  //   return this.rpcRequest({ method: 'server.proxy.set-outbound', params })
  // }

  // marketplace URLs

  async checkOSUpdate(
    params: RR.CheckOsUpdateReq,
  ): Promise<RR.CheckOsUpdateRes> {
    return this.rpcRequest({
      method: 'registry.os.version.get',
      params,
    })
  }

  async getRegistryInfo(
    params: RR.GetRegistryInfoReq,
  ): Promise<RR.GetRegistryInfoRes> {
    return this.rpcRequest({
      method: 'registry.info',
      params,
    })
  }

  async getRegistryPackage(
    params: RR.GetRegistryPackageReq,
  ): Promise<RR.GetRegistryPackageRes> {
    return this.rpcRequest({
      method: 'registry.package.get',
      params,
    })
  }

  async getRegistryPackages(
    params: RR.GetRegistryPackagesReq,
  ): Promise<RR.GetRegistryPackagesRes> {
    return this.rpcRequest({
      method: 'registry.package.get',
      params,
    })
  }

  // notification

  async getNotifications(
    params: RR.GetNotificationsReq,
  ): Promise<RR.GetNotificationsRes> {
    return this.rpcRequest({ method: 'notification.list', params })
  }

  async deleteNotifications(
    params: RR.DeleteNotificationsReq,
  ): Promise<RR.DeleteNotificationsRes> {
    return this.rpcRequest({ method: 'notification.remove', params })
  }

  async markSeenNotifications(
    params: RR.MarkSeenNotificationReq,
  ): Promise<RR.MarkSeenNotificationRes> {
    return this.rpcRequest({ method: 'notification.mark-seen', params })
  }

  async markSeenAllNotifications(
    params: RR.MarkSeenAllNotificationsReq,
  ): Promise<RR.MarkSeenAllNotificationsRes> {
    return this.rpcRequest({
      method: 'notification.mark-seen-before',
      params,
    })
  }

  async markUnseenNotifications(
    params: RR.MarkUnseenNotificationReq,
  ): Promise<RR.MarkUnseenNotificationRes> {
    return this.rpcRequest({ method: 'notification.mark-unseen', params })
  }

  // proxies

  // async addProxy(params: RR.AddProxyReq): Promise<RR.AddProxyRes> {
  //   return this.rpcRequest({ method: 'net.proxy.add', params })
  // }

  // async updateProxy(params: RR.UpdateProxyReq): Promise<RR.UpdateProxyRes> {
  //   return this.rpcRequest({ method: 'net.proxy.update', params })
  // }

  // async deleteProxy(params: RR.DeleteProxyReq): Promise<RR.DeleteProxyRes> {
  //   return this.rpcRequest({ method: 'net.proxy.delete', params })
  // }

  // domains

  // async claimStart9ToDomain(
  //   params: RR.ClaimStart9ToReq,
  // ): Promise<RR.ClaimStart9ToRes> {
  //   return this.rpcRequest({ method: 'net.domain.me.claim', params })
  // }

  // async deleteStart9ToDomain(
  //   params: RR.DeleteStart9ToReq,
  // ): Promise<RR.DeleteStart9ToRes> {
  //   return this.rpcRequest({ method: 'net.domain.me.delete', params })
  // }

  // async addDomain(params: RR.AddDomainReq): Promise<RR.AddDomainRes> {
  //   return this.rpcRequest({ method: 'net.domain.add', params })
  // }

  // async deleteDomain(params: RR.DeleteDomainReq): Promise<RR.DeleteDomainRes> {
  //   return this.rpcRequest({ method: 'net.domain.delete', params })
  // }

  // port forwards

  // async overridePortForward(
  //   params: RR.OverridePortReq,
  // ): Promise<RR.OverridePortRes> {
  //   return this.rpcRequest({ method: 'net.port-forwards.override', params })
  // }

  // wifi

  async enableWifi(params: RR.EnabledWifiReq): Promise<RR.EnabledWifiRes> {
    return this.rpcRequest({ method: 'wifi.enable', params })
  }

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
    return this.rpcRequest({ method: 'wifi.remove', params })
  }

  // smtp

  async setSmtp(params: RR.SetSMTPReq): Promise<RR.SetSMTPRes> {
    return this.rpcRequest({ method: 'server.set-smtp', params })
  }

  async clearSmtp(params: RR.ClearSMTPReq): Promise<RR.ClearSMTPRes> {
    return this.rpcRequest({ method: 'server.clear-smtp', params })
  }

  async testSmtp(params: RR.TestSMTPReq): Promise<RR.TestSMTPRes> {
    return this.rpcRequest({ method: 'server.test-smtp', params })
  }

  // ssh

  async getSshKeys(params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes> {
    return this.rpcRequest({ method: 'ssh.list', params })
  }

  async addSshKey(params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes> {
    return this.rpcRequest({ method: 'ssh.add', params })
  }

  async deleteSshKey(params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes> {
    return this.rpcRequest({ method: 'ssh.remove', params })
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

  // async addBackupTarget(
  //   type: BackupTargetType,
  //   params: RR.AddCifsBackupTargetReq | RR.AddCloudBackupTargetReq,
  // ): Promise<RR.AddBackupTargetRes> {
  //   params.path = params.path.replace('/\\/g', '/')
  //   return this.rpcRequest({ method: `backup.target.${type}.add`, params })
  // }

  // async updateBackupTarget(
  //   type: BackupTargetType,
  //   params: RR.UpdateCifsBackupTargetReq | RR.UpdateCloudBackupTargetReq,
  // ): Promise<RR.UpdateBackupTargetRes> {
  //   return this.rpcRequest({ method: `backup.target.${type}.update`, params })
  // }

  // async removeBackupTarget(
  //   params: RR.RemoveBackupTargetReq,
  // ): Promise<RR.RemoveBackupTargetRes> {
  //   return this.rpcRequest({ method: 'backup.target.remove', params })
  // }

  // async getBackupJobs(
  //   params: RR.GetBackupJobsReq,
  // ): Promise<RR.GetBackupJobsRes> {
  //   return this.rpcRequest({ method: 'backup.job.list', params })
  // }

  // async createBackupJob(
  //   params: RR.CreateBackupJobReq,
  // ): Promise<RR.CreateBackupJobRes> {
  //   return this.rpcRequest({ method: 'backup.job.create', params })
  // }

  // async updateBackupJob(
  //   params: RR.UpdateBackupJobReq,
  // ): Promise<RR.UpdateBackupJobRes> {
  //   return this.rpcRequest({ method: 'backup.job.update', params })
  // }

  // async deleteBackupJob(
  //   params: RR.DeleteBackupJobReq,
  // ): Promise<RR.DeleteBackupJobRes> {
  //   return this.rpcRequest({ method: 'backup.job.delete', params })
  // }

  // async getBackupRuns(
  //   params: RR.GetBackupRunsReq,
  // ): Promise<RR.GetBackupRunsRes> {
  //   return this.rpcRequest({ method: 'backup.runs.list', params })
  // }

  // async deleteBackupRuns(
  //   params: RR.DeleteBackupRunsReq,
  // ): Promise<RR.DeleteBackupRunsRes> {
  //   return this.rpcRequest({ method: 'backup.runs.delete', params })
  // }

  // package

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

  async installPackage(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes> {
    return this.rpcRequest({ method: 'package.install', params })
  }

  async cancelInstallPackage(
    params: RR.CancelInstallPackageReq,
  ): Promise<RR.CancelInstallPackageRes> {
    return this.rpcRequest({ method: 'package.cancel-install', params })
  }

  async getActionInput(
    params: RR.GetActionInputReq,
  ): Promise<RR.GetActionInputRes> {
    return this.rpcRequest({ method: 'package.action.get-input', params })
  }

  async runAction(params: RR.ActionReq): Promise<RR.ActionRes> {
    return this.rpcRequest({ method: 'package.action.run', params })
  }

  async restorePackages(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes> {
    return this.rpcRequest({ method: 'package.backup.restore', params })
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

  async rebuildPackage(
    params: RR.RebuildPackageReq,
  ): Promise<RR.RebuildPackageRes> {
    return this.rpcRequest({ method: 'package.rebuild', params })
  }

  async uninstallPackage(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes> {
    return this.rpcRequest({ method: 'package.uninstall', params })
  }

  async sideloadPackage(): Promise<RR.SideloadPackageRes> {
    return this.rpcRequest({
      method: 'package.sideload',
      params: {},
    })
  }

  // async setServiceOutboundProxy(
  //   params: RR.SetServiceOutboundProxyReq,
  // ): Promise<RR.SetServiceOutboundProxyRes> {
  //   return this.rpcRequest({ method: 'package.proxy.set-outbound', params })
  // }

  async removeAcme(params: RR.RemoveAcmeReq): Promise<RR.RemoveAcmeRes> {
    return this.rpcRequest({
      method: 'net.acme.remove',
      params,
    })
  }

  async initAcme(params: RR.InitAcmeReq): Promise<RR.InitAcmeRes> {
    return this.rpcRequest({
      method: 'net.acme.init',
      params,
    })
  }

  async addTorKey(params: RR.AddTorKeyReq): Promise<RR.AddTorKeyRes> {
    return this.rpcRequest({
      method: 'net.tor.key.add',
      params,
    })
  }

  async generateTorKey(params: RR.GenerateTorKeyReq): Promise<RR.AddTorKeyRes> {
    return this.rpcRequest({
      method: 'net.tor.key.generate',
      params,
    })
  }

  async serverBindingSetPubic(
    params: RR.ServerBindingSetPublicReq,
  ): Promise<RR.BindingSetPublicRes> {
    return this.rpcRequest({
      method: 'server.host.binding.set-public',
      params,
    })
  }

  async serverAddOnion(params: RR.ServerAddOnionReq): Promise<RR.AddOnionRes> {
    return this.rpcRequest({
      method: 'server.host.address.onion.add',
      params,
    })
  }

  async serverRemoveOnion(
    params: RR.ServerRemoveOnionReq,
  ): Promise<RR.RemoveOnionRes> {
    return this.rpcRequest({
      method: 'server.host.address.onion.remove',
      params,
    })
  }

  async serverAddDomain(
    params: RR.ServerAddDomainReq,
  ): Promise<RR.AddDomainRes> {
    return this.rpcRequest({
      method: 'server.host.address.domain.add',
      params,
    })
  }

  async serverRemoveDomain(
    params: RR.ServerRemoveDomainReq,
  ): Promise<RR.RemoveDomainRes> {
    return this.rpcRequest({
      method: 'server.host.address.domain.remove',
      params,
    })
  }

  async pkgBindingSetPubic(
    params: RR.PkgBindingSetPublicReq,
  ): Promise<RR.BindingSetPublicRes> {
    return this.rpcRequest({
      method: 'package.host.binding.set-public',
      params,
    })
  }

  async pkgAddOnion(params: RR.PkgAddOnionReq): Promise<RR.AddOnionRes> {
    return this.rpcRequest({
      method: 'package.host.address.onion.add',
      params,
    })
  }

  async pkgRemoveOnion(
    params: RR.PkgRemoveOnionReq,
  ): Promise<RR.RemoveOnionRes> {
    return this.rpcRequest({
      method: 'package.host.address.onion.remove',
      params,
    })
  }

  async pkgAddDomain(params: RR.PkgAddDomainReq): Promise<RR.AddDomainRes> {
    return this.rpcRequest({
      method: 'package.host.address.domain.add',
      params,
    })
  }

  async pkgRemoveDomain(
    params: RR.PkgRemoveDomainReq,
  ): Promise<RR.RemoveDomainRes> {
    return this.rpcRequest({
      method: 'package.host.address.domain.remove',
      params,
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
      let data: Uint8Array
      if (opts.responseType === 'arrayBuffer') {
        data = Buffer.from(res.body as ArrayBuffer)
      } else if (opts.responseType === 'text') {
        data = Buffer.from(res.body as string)
      } else if ((opts.responseType as string) === 'blob') {
        data = Buffer.from(await (res.body as Blob).arrayBuffer())
      } else {
        console.warn(
          `could not verify Repr-Digest for responseType ${
            opts.responseType || 'json'
          }`,
        )
        return res.body
      }
      const computedDigest = Buffer.from(blake3(data)).toString('base64')
      if (`blake3=:${computedDigest}:` === digest) return res.body
      console.debug(computedDigest, digest)
      throw new Error('File digest mismatch.')
    }
    return res.body
  }
}
