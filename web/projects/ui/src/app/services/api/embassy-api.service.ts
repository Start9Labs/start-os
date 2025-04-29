import { MarketplacePkg } from '@start9labs/marketplace'
import { T } from '@start9labs/start-sdk'
import { RR } from './api.types'
import { WebSocketSubject } from 'rxjs/webSocket'

export abstract class ApiService {
  // http

  // for sideloading packages
  abstract uploadPackage(guid: string, body: Blob): Promise<void>

  // for getting static files: ex icons, instructions, licenses
  abstract getStaticProxy(
    pkg: MarketplacePkg,
    path: 'LICENSE.md' | 'instructions.md',
  ): Promise<string>

  abstract getStaticInstalled(
    id: T.PackageId,
    path: 'LICENSE.md' | 'instructions.md',
  ): Promise<string>

  // websocket

  abstract openWebsocket$<T>(
    guid: string,
    config?: RR.WebsocketConfig<T>,
  ): WebSocketSubject<T>

  // state

  abstract echo(params: RR.EchoReq, url: string): Promise<RR.EchoRes>

  abstract getState(): Promise<RR.ServerState>

  // db

  abstract subscribeToPatchDB(
    params: RR.SubscribePatchReq,
  ): Promise<RR.SubscribePatchRes>

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

  // diagnostic

  abstract diagnosticGetError(): Promise<RR.DiagnosticErrorRes>
  abstract diagnosticRestart(): Promise<void>
  abstract diagnosticForgetDrive(): Promise<void>
  abstract diagnosticRepairDisk(): Promise<void>
  abstract diagnosticGetLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes>

  // init

  abstract initFollowProgress(): Promise<RR.InitFollowProgressRes>

  abstract initFollowLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes>

  // server

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

  abstract followServerMetrics(
    params: RR.FollowServerMetricsReq,
  ): Promise<RR.FollowServerMetricsRes>

  abstract updateServer(params: RR.UpdateServerReq): Promise<RR.UpdateServerRes>

  abstract restartServer(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes>

  abstract shutdownServer(
    params: RR.ShutdownServerReq,
  ): Promise<RR.ShutdownServerRes>

  abstract repairDisk(params: RR.DiskRepairReq): Promise<RR.DiskRepairRes>

  abstract resetTor(params: RR.ResetTorReq): Promise<RR.ResetTorRes>

  // @TODO 041

  // ** server outbound proxy **

  // abstract setOsOutboundProxy(
  //   params: RR.SetOsOutboundProxyReq,
  // ): Promise<RR.SetOsOutboundProxyRes>

  // smtp

  abstract setSmtp(params: RR.SetSMTPReq): Promise<RR.SetSMTPRes>

  abstract clearSmtp(params: RR.ClearSMTPReq): Promise<RR.ClearSMTPRes>

  abstract testSmtp(params: RR.TestSMTPReq): Promise<RR.TestSMTPRes>

  // marketplace URLs

  abstract checkOSUpdate(
    params: RR.CheckOsUpdateReq,
  ): Promise<RR.CheckOsUpdateRes>

  abstract getRegistryInfo(
    params: RR.GetRegistryInfoReq,
  ): Promise<RR.GetRegistryInfoRes>

  abstract getRegistryPackage(
    params: RR.GetRegistryPackageReq,
  ): Promise<RR.GetRegistryPackageRes>

  abstract getRegistryPackages(
    params: RR.GetRegistryPackagesReq,
  ): Promise<RR.GetRegistryPackagesRes>

  // notification

  abstract getNotifications(
    params: RR.GetNotificationsReq,
  ): Promise<RR.GetNotificationsRes>

  abstract markSeenNotifications(
    params: RR.MarkSeenNotificationReq,
  ): Promise<RR.MarkSeenNotificationRes>

  abstract markSeenAllNotifications(
    params: RR.MarkSeenAllNotificationsReq,
  ): Promise<RR.MarkSeenAllNotificationsRes>

  abstract markUnseenNotifications(
    params: RR.DeleteNotificationsReq,
  ): Promise<RR.DeleteNotificationsRes>

  abstract deleteNotifications(
    params: RR.DeleteNotificationsReq,
  ): Promise<RR.DeleteNotificationsRes>

  // ** proxies **

  // @TODO 041

  // abstract addProxy(params: RR.AddProxyReq): Promise<RR.AddProxyRes>

  // abstract updateProxy(params: RR.UpdateProxyReq): Promise<RR.UpdateProxyRes>

  // abstract deleteProxy(params: RR.DeleteProxyReq): Promise<RR.DeleteProxyRes>

  // ** domains **

  // @TODO 041

  // abstract claimStart9ToDomain(
  //   params: RR.ClaimStart9ToReq,
  // ): Promise<RR.ClaimStart9ToRes>

  // abstract deleteStart9ToDomain(
  //   params: RR.DeleteStart9ToReq,
  // ): Promise<RR.DeleteStart9ToRes>

  // abstract addDomain(params: RR.AddDomainReq): Promise<RR.AddDomainRes>

  // abstract deleteDomain(params: RR.DeleteDomainReq): Promise<RR.DeleteDomainRes>

  // ** port forwards **

  // @TODO 041

  // abstract overridePortForward(
  //   params: RR.OverridePortReq,
  // ): Promise<RR.OverridePortRes>

  // wifi

  abstract enableWifi(params: RR.EnabledWifiReq): Promise<RR.EnabledWifiRes>

  abstract setWifiCountry(
    params: RR.SetWifiCountryReq,
  ): Promise<RR.SetWifiCountryRes>

  abstract getWifi(
    params: RR.GetWifiReq,
    timeout: number,
  ): Promise<RR.GetWifiRes>

  abstract addWifi(params: RR.AddWifiReq): Promise<RR.AddWifiRes>

  abstract connectWifi(params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes>

  abstract deleteWifi(params: RR.DeleteWifiReq): Promise<RR.DeleteWifiRes>

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

  abstract createBackup(params: RR.CreateBackupReq): Promise<RR.CreateBackupRes>

  // @TODO 041

  // ** automated backups **

  // abstract addBackupTarget(
  //   type: BackupTargetType,
  //   params:
  //     | RR.AddCifsBackupTargetReq
  //     | RR.AddCloudBackupTargetReq
  //     | RR.AddDiskBackupTargetReq,
  // ): Promise<RR.AddBackupTargetRes>

  // abstract updateBackupTarget(
  //   type: BackupTargetType,
  //   params:
  //     | RR.UpdateCifsBackupTargetReq
  //     | RR.UpdateCloudBackupTargetReq
  //     | RR.UpdateDiskBackupTargetReq,
  // ): Promise<RR.UpdateBackupTargetRes>

  // abstract removeBackupTarget(
  //   params: RR.RemoveBackupTargetReq,
  // ): Promise<RR.RemoveBackupTargetRes>

  // abstract getBackupJobs(
  //   params: RR.GetBackupJobsReq,
  // ): Promise<RR.GetBackupJobsRes>

  // abstract createBackupJob(
  //   params: RR.CreateBackupJobReq,
  // ): Promise<RR.CreateBackupJobRes>

  // abstract updateBackupJob(
  //   params: RR.UpdateBackupJobReq,
  // ): Promise<RR.UpdateBackupJobRes>

  // abstract deleteBackupJob(
  //   params: RR.DeleteBackupJobReq,
  // ): Promise<RR.DeleteBackupJobRes>

  // abstract getBackupRuns(
  //   params: RR.GetBackupRunsReq,
  // ): Promise<RR.GetBackupRunsRes>

  // abstract deleteBackupRuns(
  //   params: RR.DeleteBackupRunsReq,
  // ): Promise<RR.DeleteBackupRunsRes>

  // package

  abstract getPackageLogs(
    params: RR.GetPackageLogsReq,
  ): Promise<RR.GetPackageLogsRes>

  abstract followPackageLogs(
    params: RR.FollowPackageLogsReq,
  ): Promise<RR.FollowPackageLogsRes>

  abstract installPackage(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes>

  abstract getActionInput(
    params: RR.GetActionInputReq,
  ): Promise<RR.GetActionInputRes>

  abstract runAction(params: RR.ActionReq): Promise<RR.ActionRes>

  abstract restorePackages(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes>

  abstract startPackage(params: RR.StartPackageReq): Promise<RR.StartPackageRes>

  abstract restartPackage(
    params: RR.RestartPackageReq,
  ): Promise<RR.RestartPackageRes>

  abstract stopPackage(params: RR.StopPackageReq): Promise<RR.StopPackageRes>

  abstract rebuildPackage(
    params: RR.RebuildPackageReq,
  ): Promise<RR.RebuildPackageRes>

  abstract uninstallPackage(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes>

  abstract sideloadPackage(): Promise<RR.SideloadPackageRes>

  // @TODO 041

  // ** service outbound proxy **

  // abstract setServiceOutboundProxy(
  //   params: RR.SetServiceOutboundProxyReq,
  // ): Promise<RR.SetServiceOutboundProxyRes>

  abstract initAcme(params: RR.InitAcmeReq): Promise<RR.InitAcmeRes>

  abstract removeAcme(params: RR.RemoveAcmeReq): Promise<RR.RemoveAcmeRes>

  abstract addTorKey(params: RR.AddTorKeyReq): Promise<RR.AddTorKeyRes>

  abstract generateTorKey(
    params: RR.GenerateTorKeyReq,
  ): Promise<RR.AddTorKeyRes>

  abstract serverBindingSetPubic(
    params: RR.ServerBindingSetPublicReq,
  ): Promise<RR.BindingSetPublicRes>

  abstract serverAddOnion(params: RR.ServerAddOnionReq): Promise<RR.AddOnionRes>

  abstract serverRemoveOnion(
    params: RR.ServerRemoveOnionReq,
  ): Promise<RR.RemoveOnionRes>

  abstract serverAddDomain(
    params: RR.ServerAddDomainReq,
  ): Promise<RR.AddDomainRes>

  abstract serverRemoveDomain(
    params: RR.ServerRemoveDomainReq,
  ): Promise<RR.RemoveDomainRes>

  abstract pkgBindingSetPubic(
    params: RR.PkgBindingSetPublicReq,
  ): Promise<RR.BindingSetPublicRes>

  abstract pkgAddOnion(params: RR.PkgAddOnionReq): Promise<RR.AddOnionRes>

  abstract pkgRemoveOnion(
    params: RR.PkgRemoveOnionReq,
  ): Promise<RR.RemoveOnionRes>

  abstract pkgAddDomain(params: RR.PkgAddDomainReq): Promise<RR.AddDomainRes>

  abstract pkgRemoveDomain(
    params: RR.PkgRemoveDomainReq,
  ): Promise<RR.RemoveDomainRes>
}
