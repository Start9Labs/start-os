import { Observable } from 'rxjs'
import { RR } from './api.types'
import { RPCOptions } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  GetPackageRes,
  GetPackagesRes,
  MarketplacePkg,
} from '@start9labs/marketplace'
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

  abstract initGetProgress(): Promise<RR.InitGetProgressRes>

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

  abstract repairDisk(params: RR.DiskRepairReq): Promise<RR.DiskRepairRes>

  abstract resetTor(params: RR.ResetTorReq): Promise<RR.ResetTorRes>

  // marketplace URLs

  abstract registryRequest<T>(
    registryUrl: string,
    options: RPCOptions,
  ): Promise<T>

  abstract checkOSUpdate(qp: RR.CheckOSUpdateReq): Promise<RR.CheckOSUpdateRes>

  abstract getRegistryInfo(registryUrl: string): Promise<T.RegistryInfo>

  abstract getRegistryPackage(
    url: string,
    id: string,
    versionRange: string | null,
  ): Promise<GetPackageRes>

  abstract getRegistryPackages(registryUrl: string): Promise<GetPackagesRes>

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

  abstract createBackup(params: RR.CreateBackupReq): Promise<RR.CreateBackupRes>

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
}
