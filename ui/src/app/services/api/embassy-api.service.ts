import { Subject, Observable } from 'rxjs'
import { Http, Update, Operation, Revision, Source, Store } from 'patch-db-client'
import { RR } from './api.types'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { RequestError } from '../http.service'

export abstract class ApiService implements Source<DataModel>, Http<DataModel> {
  protected readonly sync = new Subject<Update<DataModel>>()

  /** PatchDb Source interface. Post/Patch requests provide a source of patches to the db. */
  // sequenceStream '_' is not used by the live api, but is overridden by the mock
  watch$ (_?: Store<DataModel>): Observable<Update<DataModel>> {
    return this.sync.asObservable()
  }

  connectionMade$ = new Subject<void>()

  // for getting static files: ex icons, instructions, licenses
  abstract getStatic (url: string): Promise<string>

  // db

  abstract getRevisions (since: number): Promise<RR.GetRevisionsRes>

  abstract getDump (): Promise<RR.GetDumpRes>

  protected abstract setDbValueRaw (params: RR.SetDBValueReq): Promise<RR.SetDBValueRes>
  setDbValue = (params: RR.SetDBValueReq) => this.syncResponse(
    () => this.setDbValueRaw(params),
  )()

  // auth

  abstract login (params: RR.LoginReq): Promise<RR.loginRes>

  abstract logout (params: RR.LogoutReq): Promise<RR.LogoutRes>

  abstract getSessions (params: RR.GetSessionsReq): Promise<RR.GetSessionsRes>

  abstract killSessions (params: RR.KillSessionsReq): Promise<RR.KillSessionsRes>

  // server

  protected abstract setShareStatsRaw (params: RR.SetShareStatsReq): Promise<RR.SetShareStatsRes>
  setShareStats = (params: RR.SetShareStatsReq) => this.syncResponse(
    () => this.setShareStatsRaw(params),
  )()

  abstract getServerLogs (params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes>

  abstract getServerMetrics (params: RR.GetServerMetricsReq): Promise<RR.GetServerMetricsRes>

  abstract getPkgMetrics (params: RR.GetPackageMetricsReq): Promise<RR.GetPackageMetricsRes>

  protected abstract updateServerRaw (params: RR.UpdateServerReq): Promise<RR.UpdateServerRes>
  updateServer = (params: RR.UpdateServerReq) => this.syncResponse(
    () => this.updateServerWrapper(params),
  )()
  async updateServerWrapper (params: RR.UpdateServerReq) {
    const res = await this.updateServerRaw(params)
    if (res.response === 'no-updates') {
      throw new Error('Could ont find a newer version of EmbassyOS')
    }
    return res
  }

  abstract restartServer (params: RR.UpdateServerReq): Promise<RR.RestartServerRes>

  abstract shutdownServer (params: RR.ShutdownServerReq): Promise<RR.ShutdownServerRes>

  // marketplace URLs

  abstract getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes>

  abstract getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes>

  abstract getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes>

  abstract getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes>

  abstract getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes>

  // protected abstract setPackageMarketplaceRaw (params: RR.SetPackageMarketplaceReq): Promise<RR.SetPackageMarketplaceRes>
  // setPackageMarketplace = (params: RR.SetPackageMarketplaceReq) => this.syncResponse(
  //   () => this.setPackageMarketplaceRaw(params),
  // )()

  // password
  // abstract updatePassword (params: RR.UpdatePasswordReq): Promise<RR.UpdatePasswordRes>

  // notification

  abstract getNotificationsRaw (params: RR.GetNotificationsReq): Promise<RR.GetNotificationsRes>
  getNotifications = (params: RR.GetNotificationsReq) => this.syncResponse<RR.GetNotificationsRes['response'], any>(
    () => this.getNotificationsRaw(params),
  )()

  abstract deleteNotification (params: RR.DeleteNotificationReq): Promise<RR.DeleteNotificationRes>

  abstract deleteAllNotifications (params: RR.DeleteAllNotificationsReq): Promise<RR.DeleteAllNotificationsRes>

  // wifi

  abstract getWifi (params: RR.GetWifiReq, timeout: number): Promise<RR.GetWifiRes>

  abstract setWifiCountry (params: RR.SetWifiCountryReq): Promise<RR.SetWifiCountryRes>

  abstract addWifi (params: RR.AddWifiReq): Promise<RR.AddWifiRes>

  abstract connectWifi (params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes>

  abstract deleteWifi (params: RR.DeleteWifiReq): Promise<RR.ConnectWifiRes>

  // ssh

  abstract getSshKeys (params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes>

  abstract addSshKey (params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes>

  abstract deleteSshKey (params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes>

  // backup

  protected abstract createBackupRaw (params: RR.CreateBackupReq): Promise<RR.CreateBackupRes>
  createBackup = (params: RR.CreateBackupReq) => this.syncResponse(
    () => this.createBackupRaw(params),
  )()

  // disk

  abstract getDisks (params: RR.GetDisksReq): Promise<RR.GetDisksRes>

  abstract getBackupInfo (params: RR.GetBackupInfoReq): Promise<RR.GetBackupInfoRes>

  // package

  abstract getPackageProperties (params: RR.GetPackagePropertiesReq): Promise<RR.GetPackagePropertiesRes<any>['data']>

  abstract getPackageLogs (params: RR.GetPackageLogsReq): Promise<RR.GetPackageLogsRes>

  protected abstract installPackageRaw (params: RR.InstallPackageReq): Promise<RR.InstallPackageRes>
  installPackage = (params: RR.InstallPackageReq) => this.syncResponse(
    () => this.installPackageRaw(params),
  )()

  abstract dryUpdatePackage (params: RR.DryUpdatePackageReq): Promise<RR.DryUpdatePackageRes>

  abstract getPackageConfig (params: RR.GetPackageConfigReq): Promise<RR.GetPackageConfigRes>

  abstract drySetPackageConfig (params: RR.DrySetPackageConfigReq): Promise<RR.DrySetPackageConfigRes>

  protected abstract setPackageConfigRaw (params: RR.SetPackageConfigReq): Promise<RR.SetPackageConfigRes>
  setPackageConfig = (params: RR.SetPackageConfigReq) => this.syncResponse(
    () => this.setPackageConfigRaw(params),
  )()

  protected abstract restorePackageRaw (params: RR.RestorePackageReq): Promise<RR.RestorePackageRes>
  restorePackage = (params: RR.RestorePackageReq) => this.syncResponse(
    () => this.restorePackageRaw(params),
  )()

  abstract executePackageAction (params: RR.ExecutePackageActionReq): Promise<RR.ExecutePackageActionRes>

  protected abstract startPackageRaw (params: RR.StartPackageReq): Promise<RR.StartPackageRes>
  startPackage = (params: RR.StartPackageReq) => this.syncResponse(
    () => this.startPackageRaw(params),
  )()

  abstract dryStopPackage (params: RR.DryStopPackageReq): Promise<RR.DryStopPackageRes>

  protected abstract stopPackageRaw (params: RR.StopPackageReq): Promise<RR.StopPackageRes>
  stopPackage = (params: RR.StopPackageReq) => this.syncResponse(
    () => this.stopPackageRaw(params),
  )()

  abstract dryUninstallPackage (params: RR.DryUninstallPackageReq): Promise<RR.DryUninstallPackageRes>

  protected abstract uninstallPackageRaw (params: RR.UninstallPackageReq): Promise<RR.UninstallPackageRes>
  uninstallPackage = (params: RR.UninstallPackageReq) => this.syncResponse(
    () => this.uninstallPackageRaw(params),
  )()

  abstract dryConfigureDependency (params: RR.DryConfigureDependencyReq): Promise<RR.DryConfigureDependencyRes>

  protected abstract deleteRecoveredPackageRaw (params: RR.UninstallPackageReq): Promise<RR.UninstallPackageRes>
  deleteRecoveredPackage = (params: RR.UninstallPackageReq) => this.syncResponse(
    () => this.deleteRecoveredPackageRaw(params),
  )()


  // Helper allowing quick decoration to sync the response patch and return the response contents.
  // Pass in a tempUpdate function which returns a UpdateTemp corresponding to a temporary
  // state change you'd like to enact prior to request and expired when request terminates.
  private syncResponse<T, F extends (...args: any[]) => Promise<{ response: T, revision?: Revision }>> (f: F, temp?: Operation): (...args: Parameters<F>) => Promise<T> {
    return (...a) => {
      // let expireId = undefined
      // if (temp) {
      //   expireId = uuid.v4()
      //   this.sync.next({ patch: [temp], expiredBy: expireId })
      // }

      return f(a)
        .catch((e: RequestError) => {
          if (e.revision) this.sync.next(e.revision)
          throw e
        })
        .then(({ response, revision }) => {
          this.connectionMade$.next()
          if (revision) this.sync.next(revision)
          return response
        })
    }
  }
}
