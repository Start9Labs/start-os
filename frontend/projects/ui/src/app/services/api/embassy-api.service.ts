import { Subject, Observable } from 'rxjs'
import {
  Http,
  Update,
  Operation,
  Revision,
  Source,
  Store,
  RPCResponse,
} from 'patch-db-client'
import { RR } from './api.types'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { RequestError } from '../http.service'
import { map } from 'rxjs/operators'

export abstract class ApiService implements Source<DataModel>, Http<DataModel> {
  protected readonly sync$ = new Subject<Update<DataModel>>()

  /** PatchDb Source interface. Post/Patch requests provide a source of patches to the db. */
  // sequenceStream '_' is not used by the live api, but is overridden by the mock
  watch$(_?: Store<DataModel>): Observable<RPCResponse<Update<DataModel>>> {
    return this.sync$
      .asObservable()
      .pipe(map(result => ({ result, jsonrpc: '2.0' })))
  }

  // for getting static files: ex icons, instructions, licenses
  abstract getStatic(url: string): Promise<string>

  // db

  abstract getRevisions(since: number): Promise<RR.GetRevisionsRes>

  abstract getDump(): Promise<RR.GetDumpRes>

  protected abstract setDbValueRaw(
    params: RR.SetDBValueReq,
  ): Promise<RR.SetDBValueRes>
  setDbValue = (params: RR.SetDBValueReq) =>
    this.syncResponse(() => this.setDbValueRaw(params))()

  // auth

  abstract login(params: RR.LoginReq): Promise<RR.loginRes>

  abstract logout(params: RR.LogoutReq): Promise<RR.LogoutRes>

  abstract getSessions(params: RR.GetSessionsReq): Promise<RR.GetSessionsRes>

  abstract killSessions(params: RR.KillSessionsReq): Promise<RR.KillSessionsRes>

  // server

  abstract getServerLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes>

  abstract getServerMetrics(
    params: RR.GetServerMetricsReq,
  ): Promise<RR.GetServerMetricsRes>

  abstract getPkgMetrics(
    params: RR.GetPackageMetricsReq,
  ): Promise<RR.GetPackageMetricsRes>

  protected abstract updateServerRaw(
    params: RR.UpdateServerReq,
  ): Promise<RR.UpdateServerRes>
  updateServer = (params: RR.UpdateServerReq) =>
    this.syncResponse(() => this.updateServerWrapper(params))()
  async updateServerWrapper(params: RR.UpdateServerReq) {
    const res = await this.updateServerRaw(params)
    if (res.response === 'no-updates') {
      throw new Error('Could ont find a newer version of EmbassyOS')
    }
    return res
  }

  abstract restartServer(
    params: RR.UpdateServerReq,
  ): Promise<RR.RestartServerRes>

  abstract shutdownServer(
    params: RR.ShutdownServerReq,
  ): Promise<RR.ShutdownServerRes>

  abstract systemRebuild(
    params: RR.SystemRebuildReq,
  ): Promise<RR.SystemRebuildRes>

  // marketplace URLs

  abstract marketplaceProxy<T>(
    path: string,
    params: {},
    url: string,
  ): Promise<T>

  abstract getEos(
    params: RR.GetMarketplaceEOSReq,
  ): Promise<RR.GetMarketplaceEOSRes>

  // password
  // abstract updatePassword (params: RR.UpdatePasswordReq): Promise<RR.UpdatePasswordRes>

  // notification

  abstract getNotificationsRaw(
    params: RR.GetNotificationsReq,
  ): Promise<RR.GetNotificationsRes>
  getNotifications = (params: RR.GetNotificationsReq) =>
    this.syncResponse<RR.GetNotificationsRes['response'], any>(() =>
      this.getNotificationsRaw(params),
    )()

  abstract deleteNotification(
    params: RR.DeleteNotificationReq,
  ): Promise<RR.DeleteNotificationRes>

  abstract deleteAllNotifications(
    params: RR.DeleteAllNotificationsReq,
  ): Promise<RR.DeleteAllNotificationsRes>

  // wifi

  abstract getWifi(
    params: RR.GetWifiReq,
    timeout: number,
  ): Promise<RR.GetWifiRes>

  abstract setWifiCountry(
    params: RR.SetWifiCountryReq,
  ): Promise<RR.SetWifiCountryRes>

  abstract addWifi(params: RR.AddWifiReq): Promise<RR.AddWifiRes>

  abstract connectWifi(params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes>

  abstract deleteWifi(params: RR.DeleteWifiReq): Promise<RR.ConnectWifiRes>

  // ssh

  abstract getSshKeys(params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes>

  abstract addSshKey(params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes>

  abstract deleteSshKey(params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes>

  // backup

  abstract getBackupTargets(
    params: RR.GetBackupTargetsReq,
  ): Promise<RR.GetBackupTargetsRes>

  abstract addBackupTarget(
    params: RR.AddBackupTargetReq,
  ): Promise<RR.AddBackupTargetRes>

  abstract updateBackupTarget(
    params: RR.UpdateBackupTargetReq,
  ): Promise<RR.UpdateBackupTargetRes>

  abstract removeBackupTarget(
    params: RR.RemoveBackupTargetReq,
  ): Promise<RR.RemoveBackupTargetRes>

  abstract getBackupInfo(
    params: RR.GetBackupInfoReq,
  ): Promise<RR.GetBackupInfoRes>

  protected abstract createBackupRaw(
    params: RR.CreateBackupReq,
  ): Promise<RR.CreateBackupRes>
  createBackup = (params: RR.CreateBackupReq) =>
    this.syncResponse(() => this.createBackupRaw(params))()

  // package

  abstract getPackageProperties(
    params: RR.GetPackagePropertiesReq,
  ): Promise<RR.GetPackagePropertiesRes<2>['data']>

  abstract getPackageLogs(
    params: RR.GetPackageLogsReq,
  ): Promise<RR.GetPackageLogsRes>

  protected abstract installPackageRaw(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes>
  installPackage = (params: RR.InstallPackageReq) =>
    this.syncResponse(() => this.installPackageRaw(params))()

  abstract dryUpdatePackage(
    params: RR.DryUpdatePackageReq,
  ): Promise<RR.DryUpdatePackageRes>

  abstract getPackageConfig(
    params: RR.GetPackageConfigReq,
  ): Promise<RR.GetPackageConfigRes>

  abstract drySetPackageConfig(
    params: RR.DrySetPackageConfigReq,
  ): Promise<RR.DrySetPackageConfigRes>

  protected abstract setPackageConfigRaw(
    params: RR.SetPackageConfigReq,
  ): Promise<RR.SetPackageConfigRes>
  setPackageConfig = (params: RR.SetPackageConfigReq) =>
    this.syncResponse(() => this.setPackageConfigRaw(params))()

  protected abstract restorePackagesRaw(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes>
  restorePackages = (params: RR.RestorePackagesReq) =>
    this.syncResponse(() => this.restorePackagesRaw(params))()

  abstract executePackageAction(
    params: RR.ExecutePackageActionReq,
  ): Promise<RR.ExecutePackageActionRes>

  protected abstract startPackageRaw(
    params: RR.StartPackageReq,
  ): Promise<RR.StartPackageRes>
  startPackage = (params: RR.StartPackageReq) =>
    this.syncResponse(() => this.startPackageRaw(params))()

  abstract dryStopPackage(
    params: RR.DryStopPackageReq,
  ): Promise<RR.DryStopPackageRes>

  protected abstract stopPackageRaw(
    params: RR.StopPackageReq,
  ): Promise<RR.StopPackageRes>
  stopPackage = (params: RR.StopPackageReq) =>
    this.syncResponse(() => this.stopPackageRaw(params))()

  abstract dryUninstallPackage(
    params: RR.DryUninstallPackageReq,
  ): Promise<RR.DryUninstallPackageRes>

  protected abstract uninstallPackageRaw(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes>
  uninstallPackage = (params: RR.UninstallPackageReq) =>
    this.syncResponse(() => this.uninstallPackageRaw(params))()

  abstract dryConfigureDependency(
    params: RR.DryConfigureDependencyReq,
  ): Promise<RR.DryConfigureDependencyRes>

  protected abstract deleteRecoveredPackageRaw(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes>
  deleteRecoveredPackage = (params: RR.UninstallPackageReq) =>
    this.syncResponse(() => this.deleteRecoveredPackageRaw(params))()

  // Helper allowing quick decoration to sync the response patch and return the response contents.
  // Pass in a tempUpdate function which returns a UpdateTemp corresponding to a temporary
  // state change you'd like to enact prior to request and expired when request terminates.
  private syncResponse<
    T,
    F extends (...args: any[]) => Promise<{ response: T; revision?: Revision }>,
  >(f: F, temp?: Operation): (...args: Parameters<F>) => Promise<T> {
    return (...a) => {
      // let expireId = undefined
      // if (temp) {
      //   expireId = uuid.v4()
      //   this.sync.next({ patch: [temp], expiredBy: expireId })
      // }

      return f(a)
        .catch((e: RequestError) => {
          if (e.revision) this.sync$.next(e.revision)
          throw e
        })
        .then(({ response, revision }) => {
          if (revision) this.sync$.next(revision)
          return response
        })
    }
  }
}
