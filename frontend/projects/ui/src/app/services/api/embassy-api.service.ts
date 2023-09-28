import { BehaviorSubject, Observable } from 'rxjs'
import { Update } from 'patch-db-client'
import { RR, BackupTargetType, Metrics } from './api.types'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Log, SetupStatus } from '@start9labs/shared'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'

export abstract class ApiService {
  readonly patchStream$ = new BehaviorSubject<Update<DataModel>[]>([])

  // http

  // for getting static files: ex icons, instructions, licenses
  abstract getStatic(url: string): Promise<string>

  // for sideloading packages
  abstract uploadPackage(guid: string, body: Blob): Promise<void>

  abstract uploadFile(body: Blob): Promise<string>

  // db

  abstract setDbValue<T>(
    pathArr: Array<string | number>,
    value: T,
  ): Promise<RR.SetDBValueRes>

  // auth

  abstract login(params: RR.LoginReq): Promise<RR.loginRes>

  abstract logout(params: RR.LogoutReq): Promise<RR.LogoutRes>

  abstract getSessions(params: RR.GetSessionsReq): Promise<RR.GetSessionsRes>

  abstract killSessions(params: RR.KillSessionsReq): Promise<RR.KillSessionsRes>

  abstract resetPassword(
    params: RR.ResetPasswordReq,
  ): Promise<RR.ResetPasswordRes>

  // server

  abstract echo(params: RR.EchoReq, urlOverride?: string): Promise<RR.EchoRes>

  abstract openPatchWebsocket$(): Observable<Update<DataModel>>

  abstract openLogsWebsocket$(
    config: WebSocketSubjectConfig<Log>,
  ): Observable<Log>

  abstract openMetricsWebsocket$(
    config: WebSocketSubjectConfig<Metrics>,
  ): Observable<Metrics>

  abstract getSystemTime(
    params: RR.GetSystemTimeReq,
  ): Promise<RR.GetSystemTimeRes>

  abstract getServerLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes>

  abstract getKernelLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes>

  abstract getTorLogs(params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes>

  abstract followServerLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes>

  abstract followKernelLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes>

  abstract followTorLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes>

  abstract getServerMetrics(
    params: RR.GetServerMetricsReq,
  ): Promise<RR.GetServerMetricsRes>

  abstract updateServer(url?: string): Promise<RR.UpdateServerRes>

  abstract restartServer(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes>

  abstract shutdownServer(
    params: RR.ShutdownServerReq,
  ): Promise<RR.ShutdownServerRes>

  abstract systemRebuild(
    params: RR.SystemRebuildReq,
  ): Promise<RR.SystemRebuildRes>

  abstract repairDisk(params: RR.SystemRebuildReq): Promise<RR.SystemRebuildRes>

  abstract resetTor(params: RR.ResetTorReq): Promise<RR.ResetTorRes>

  abstract toggleZram(params: RR.ToggleZramReq): Promise<RR.ToggleZramRes>

  // marketplace URLs

  abstract marketplaceProxy<T>(
    path: string,
    params: Record<string, unknown>,
    url: string,
  ): Promise<T>

  abstract getEos(): Promise<RR.GetMarketplaceEosRes>

  // notification

  abstract getNotifications(
    params: RR.GetNotificationsReq,
  ): Promise<RR.GetNotificationsRes>

  abstract deleteNotification(
    params: RR.DeleteNotificationReq,
  ): Promise<RR.DeleteNotificationRes>

  abstract deleteAllNotifications(
    params: RR.DeleteAllNotificationsReq,
  ): Promise<RR.DeleteAllNotificationsRes>

  // wifi

  abstract enableWifi(params: RR.EnableWifiReq): Promise<RR.EnableWifiRes>

  abstract getWifi(
    params: RR.GetWifiReq,
    timeout: number,
  ): Promise<RR.GetWifiRes>

  abstract addWifi(params: RR.AddWifiReq): Promise<RR.AddWifiRes>

  abstract connectWifi(params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes>

  abstract deleteWifi(params: RR.DeleteWifiReq): Promise<RR.ConnectWifiRes>

  // email

  abstract testEmail(params: RR.TestEmailReq): Promise<RR.TestEmailRes>

  abstract configureEmail(
    params: RR.ConfigureEmailReq,
  ): Promise<RR.ConfigureEmailRes>

  // ssh

  abstract getSshKeys(params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes>

  abstract addSshKey(params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes>

  abstract deleteSshKey(params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes>

  // backup

  abstract getBackupTargets(
    params: RR.GetBackupTargetsReq,
  ): Promise<RR.GetBackupTargetsRes>

  abstract addBackupTarget(
    type: BackupTargetType,
    params:
      | RR.AddCifsBackupTargetReq
      | RR.AddCloudBackupTargetReq
      | RR.AddDiskBackupTargetReq,
  ): Promise<RR.AddBackupTargetRes>

  abstract updateBackupTarget(
    type: BackupTargetType,
    params:
      | RR.UpdateCifsBackupTargetReq
      | RR.UpdateCloudBackupTargetReq
      | RR.UpdateDiskBackupTargetReq,
  ): Promise<RR.UpdateBackupTargetRes>

  abstract removeBackupTarget(
    params: RR.RemoveBackupTargetReq,
  ): Promise<RR.RemoveBackupTargetRes>

  abstract getBackupJobs(
    params: RR.GetBackupJobsReq,
  ): Promise<RR.GetBackupJobsRes>

  abstract createBackupJob(
    params: RR.CreateBackupJobReq,
  ): Promise<RR.CreateBackupJobRes>

  abstract updateBackupJob(
    params: RR.UpdateBackupJobReq,
  ): Promise<RR.UpdateBackupJobRes>

  abstract deleteBackupJob(
    params: RR.DeleteBackupJobReq,
  ): Promise<RR.DeleteBackupJobRes>

  abstract getBackupRuns(
    params: RR.GetBackupRunsReq,
  ): Promise<RR.GetBackupRunsRes>

  abstract deleteBackupRuns(
    params: RR.DeleteBackupRunsReq,
  ): Promise<RR.DeleteBackupRunsRes>

  abstract getBackupInfo(
    params: RR.GetBackupInfoReq,
  ): Promise<RR.GetBackupInfoRes>

  abstract createBackup(params: RR.CreateBackupReq): Promise<RR.CreateBackupRes>

  // package

  abstract getPackageCredentials(
    params: RR.GetPackageCredentialsReq,
  ): Promise<RR.GetPackageCredentialsRes>

  abstract getPackageLogs(
    params: RR.GetPackageLogsReq,
  ): Promise<RR.GetPackageLogsRes>

  abstract followPackageLogs(
    params: RR.FollowPackageLogsReq,
  ): Promise<RR.FollowPackageLogsRes>

  abstract installPackage(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes>

  abstract getPackageConfig(
    params: RR.GetPackageConfigReq,
  ): Promise<RR.GetPackageConfigRes>

  abstract drySetPackageConfig(
    params: RR.DrySetPackageConfigReq,
  ): Promise<RR.DrySetPackageConfigRes>

  abstract setPackageConfig(
    params: RR.SetPackageConfigReq,
  ): Promise<RR.SetPackageConfigRes>

  abstract restorePackages(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes>

  abstract executePackageAction(
    params: RR.ExecutePackageActionReq,
  ): Promise<RR.ExecutePackageActionRes>

  abstract startPackage(params: RR.StartPackageReq): Promise<RR.StartPackageRes>

  abstract restartPackage(
    params: RR.RestartPackageReq,
  ): Promise<RR.RestartPackageRes>

  abstract stopPackage(params: RR.StopPackageReq): Promise<RR.StopPackageRes>

  abstract uninstallPackage(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes>

  abstract dryConfigureDependency(
    params: RR.DryConfigureDependencyReq,
  ): Promise<RR.DryConfigureDependencyRes>

  abstract sideloadPackage(
    params: RR.SideloadPackageReq,
  ): Promise<RR.SideloadPacakgeRes>

  abstract getSetupStatus(): Promise<SetupStatus | null>

  abstract followLogs(): Promise<string>
}
