import { Injectable } from '@angular/core'
import { pauseFor, Log, RPCErrorDetails } from '@start9labs/shared'
import { ApiService } from './embassy-api.service'
import {
  AddOperation,
  Operation,
  PatchOp,
  pathFromArray,
  RemoveOperation,
  ReplaceOperation,
  Revision,
} from 'patch-db-client'
import {
  InstallingState,
  PackageDataEntry,
  StateInfo,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { CifsBackupTarget, RR } from './api.types'
import { Mock } from './api.fixures'
import { from, interval, map, shareReplay, startWith, Subject, tap } from 'rxjs'
import { mockPatchData } from './mock-patch'
import { AuthService } from '../auth.service'
import { T } from '@start9labs/start-sdk'
import { MarketplacePkg } from '@start9labs/marketplace'
import { WebSocketSubject } from 'rxjs/webSocket'
import { toAuthorityUrl } from 'src/app/utils/acme'

import markdown from './md-sample.md'

const PROGRESS: T.FullProgress = {
  overall: {
    done: 0,
    total: 120,
    units: 'bytes',
  },
  phases: [
    {
      name: 'Downloading',
      progress: {
        done: 0,
        total: 40,
        units: 'bytes',
      },
    },
    {
      name: 'Validating',
      progress: null,
    },
    {
      name: 'Installing',
      progress: null,
    },
  ],
}

@Injectable()
export class MockApiService extends ApiService {
  readonly mockWsSource$ = new Subject<Revision>()
  private readonly revertTime = 1800
  sequence = 0

  constructor(private readonly auth: AuthService) {
    super()
    this.auth.isVerified$
      .pipe(
        tap(() => {
          this.sequence = 0
        }),
      )
      .subscribe()
  }

  async uploadFile(guid: string, body: Blob): Promise<void> {
    await pauseFor(2000)
  }

  async getStatic(
    urls: string[],
    params: Record<string, string | number>,
  ): Promise<string> {
    console.log('here')
    await pauseFor(2000)
    return markdown
  }

  // websocket

  openWebsocket$<T>(
    guid: string,
    config: RR.WebsocketConfig<T> = {},
  ): WebSocketSubject<T> {
    if (guid === 'db-guid') {
      return this.mockWsSource$.pipe<any>(
        shareReplay({ bufferSize: 1, refCount: true }),
      ) as WebSocketSubject<T>
    } else if (guid === 'logs-guid') {
      return interval(50).pipe<any>(
        map((_, index) => {
          // mock fire open observer
          if (index === 0) config.openObserver?.next(new Event(''))
          if (index === 100) throw new Error('HAAHHA')
          return Mock.ServerLogs[0]
        }),
      ) as WebSocketSubject<T>
    } else if (guid === 'init-progress-guid') {
      return from(this.initProgress()).pipe(
        startWith(PROGRESS),
      ) as WebSocketSubject<T>
    } else if (guid === 'sideload-progress-guid') {
      config.openObserver?.next(new Event(''))
      return from(this.initProgress()).pipe(
        startWith(PROGRESS),
      ) as WebSocketSubject<T>
    } else if (guid === 'metrics-guid') {
      return interval(1000).pipe(
        map(() => Mock.getMetrics()),
      ) as WebSocketSubject<T>
    } else {
      throw new Error('invalid guid type')
    }
  }

  // state

  async echo(params: RR.EchoReq, url: string): Promise<RR.EchoRes> {
    if (url) {
      const num = Math.floor(Math.random() * 10) + 1
      if (num > 8) return params.message
      throw new Error()
    }
    await pauseFor(2000)
    return params.message
  }

  private stateIndex = 0
  async getState(): Promise<RR.ServerState> {
    await pauseFor(1000)

    this.stateIndex++

    return this.stateIndex === 1 ? 'running' : 'running'
  }

  // db

  async subscribeToPatchDB(
    params: RR.SubscribePatchReq,
  ): Promise<RR.SubscribePatchRes> {
    await pauseFor(2000)
    return {
      dump: { id: 1, value: mockPatchData },
      guid: 'db-guid',
    }
  }

  async setDbValue<T>(
    pathArr: Array<string | number>,
    value: T,
  ): Promise<RR.SetDBValueRes> {
    const pointer = pathFromArray(pathArr)
    const params: RR.SetDBValueReq<T> = { pointer, value }
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/ui' + params.pointer,
        value: params.value,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  // auth

  async login(params: RR.LoginReq): Promise<RR.loginRes> {
    await pauseFor(2000)
    return null
  }

  async logout(params: RR.LogoutReq): Promise<RR.LogoutRes> {
    await pauseFor(2000)
    return null
  }

  async getSessions(params: RR.GetSessionsReq): Promise<RR.GetSessionsRes> {
    await pauseFor(2000)
    return Mock.Sessions
  }

  async killSessions(params: RR.KillSessionsReq): Promise<RR.KillSessionsRes> {
    await pauseFor(2000)
    return null
  }

  async resetPassword(
    params: RR.ResetPasswordReq,
  ): Promise<RR.ResetPasswordRes> {
    await pauseFor(2000)
    return null
  }

  // diagnostic

  async getError(): Promise<RPCErrorDetails> {
    await pauseFor(1000)
    return {
      code: 15,
      message: 'Unknown server',
      data: { details: 'Some details about the error here' },
    }
  }

  async diagnosticGetError(): Promise<RR.DiagnosticErrorRes> {
    await pauseFor(1000)
    return {
      code: 15,
      message: 'Unknown server',
      data: { details: 'Some details about the error here' },
    }
  }

  async diagnosticRestart(): Promise<void> {
    await pauseFor(1000)
  }

  async diagnosticForgetDrive(): Promise<void> {
    await pauseFor(1000)
  }

  async diagnosticRepairDisk(): Promise<void> {
    await pauseFor(1000)
  }

  async diagnosticGetLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    return this.getServerLogs(params)
  }

  // init

  async initFollowProgress(): Promise<RR.InitFollowProgressRes> {
    await pauseFor(250)
    return {
      progress: PROGRESS,
      guid: 'init-progress-guid',
    }
  }

  async initFollowLogs(): Promise<RR.FollowServerLogsRes> {
    await pauseFor(2000)
    return {
      startCursor: 'start-cursor',
      guid: 'logs-guid',
    }
  }

  // server

  async getSystemTime(
    params: RR.GetSystemTimeReq,
  ): Promise<RR.GetSystemTimeRes> {
    await pauseFor(2000)
    return {
      now: new Date().toUTCString(),
      uptime: 1234567,
    }
  }

  async getServerLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    await pauseFor(2000)
    const entries = this.randomLogs(params.limit)

    return {
      entries,
      startCursor: 'start-cursor',
      endCursor: 'end-cursor',
    }
  }

  async getTorLogs(params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes> {
    await pauseFor(2000)
    const entries = this.randomLogs(params.limit)

    return {
      entries,
      startCursor: 'start-cursor',
      endCursor: 'end-cursor',
    }
  }

  async getKernelLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    await pauseFor(2000)
    const entries = this.randomLogs(params.limit)

    return {
      entries,
      startCursor: 'start-cursor',
      endCursor: 'end-cursor',
    }
  }

  async followServerLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    await pauseFor(2000)
    return {
      startCursor: 'start-cursor',
      guid: 'logs-guid',
    }
  }

  async followTorLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    await pauseFor(2000)
    return {
      startCursor: 'start-cursor',
      guid: 'logs-guid',
    }
  }

  async followKernelLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    await pauseFor(2000)
    return {
      startCursor: 'start-cursor',
      guid: 'logs-guid',
    }
  }

  private randomLogs(limit = 1): Log[] {
    const arrLength = Math.ceil(limit / Mock.ServerLogs.length)
    const logs = new Array(arrLength)
      .fill(Mock.ServerLogs)
      .reduce((acc, val) => acc.concat(val), [])

    return logs
  }

  async followServerMetrics(
    params: RR.FollowServerMetricsReq,
  ): Promise<RR.FollowServerMetricsRes> {
    await pauseFor(2000)
    return {
      guid: 'metrics-guid',
      metrics: Mock.getMetrics(),
    }
  }

  async updateServer(params?: RR.UpdateServerReq): Promise<RR.UpdateServerRes> {
    await pauseFor(2000)
    const initialProgress = {
      size: null,
      downloaded: 0,
    }

    setTimeout(() => {
      this.updateOSProgress()
    }, 500)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/statusInfo/updateProgress',
        value: initialProgress,
      },
    ]
    this.mockRevision(patch)

    return 'updating'
  }

  async restartServer(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/statusInfo/restarting',
        value: true,
      },
    ]
    this.mockRevision(patch)

    setTimeout(() => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: '/serverInfo/statusInfo/restarting',
          value: false,
        },
      ]
      this.mockRevision(patch2)
    }, 2000)

    return null
  }

  async shutdownServer(
    params: RR.ShutdownServerReq,
  ): Promise<RR.ShutdownServerRes> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/statusInfo/shuttingDown',
        value: true,
      },
    ]
    this.mockRevision(patch)

    setTimeout(() => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: '/serverInfo/statusInfo/shuttingDown',
          value: false,
        },
      ]
      this.mockRevision(patch2)
    }, 2000)

    return null
  }

  async repairDisk(params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    await pauseFor(2000)
    return null
  }

  async toggleKiosk(enable: boolean): Promise<null> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/kiosk',
        value: enable,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async setDns(params: RR.SetDnsReq): Promise<RR.SetDnsRes> {
    await pauseFor(2000)

    const patch: ReplaceOperation<T.DnsSettings['staticServers']>[] = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/network/dns/staticServers',
        value: params.servers,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async queryDns(params: RR.QueryDnsReq): Promise<RR.QueryDnsRes> {
    await pauseFor(2000)

    return null
  }

  async resetTor(params: RR.ResetTorReq): Promise<RR.ResetTorRes> {
    await pauseFor(2000)
    return null
  }

  // marketplace URLs

  async checkOSUpdate(
    params: RR.CheckOsUpdateReq,
  ): Promise<RR.CheckOsUpdateRes> {
    await pauseFor(2000)
    return Mock.RegistryOSUpdate
  }

  async getRegistryInfo(
    params: RR.GetRegistryInfoReq,
  ): Promise<RR.GetRegistryInfoRes> {
    await pauseFor(2000)
    return Mock.RegistryInfo
  }

  async getRegistryPackage(
    params: RR.GetRegistryPackageReq,
  ): Promise<RR.GetRegistryPackageRes> {
    await pauseFor(2000)

    const { targetVersion, id } = params

    if (!targetVersion || targetVersion === '=*') {
      return Mock.RegistryPackages[id]!
    } else {
      return Mock.OtherPackageVersions[id]![targetVersion]!
    }
  }

  async getRegistryPackages(
    params: RR.GetRegistryPackagesReq,
  ): Promise<RR.GetRegistryPackagesRes> {
    await pauseFor(2000)
    return Mock.RegistryPackages
  }

  // notification

  async getNotifications(
    params: RR.GetNotificationsReq,
  ): Promise<RR.GetNotificationsRes> {
    await pauseFor(2000)

    return Mock.Notifications
  }

  async deleteNotifications(
    params: RR.DeleteNotificationsReq,
  ): Promise<RR.DeleteNotificationsRes> {
    await pauseFor(2000)
    return null
  }

  async markSeenNotifications(
    params: RR.MarkSeenNotificationReq,
  ): Promise<RR.MarkSeenNotificationRes> {
    await pauseFor(2000)
    return null
  }

  async markSeenAllNotifications(
    params: RR.MarkSeenAllNotificationsReq,
  ): Promise<RR.MarkSeenAllNotificationsRes> {
    await pauseFor(2000)
    return null
  }

  async markUnseenNotifications(
    params: RR.MarkUnseenNotificationReq,
  ): Promise<RR.MarkUnseenNotificationRes> {
    await pauseFor(2000)
    return null
  }

  // proxies

  private proxyId = 0
  async addTunnel(params: RR.AddTunnelReq): Promise<RR.AddTunnelRes> {
    await pauseFor(2000)

    const id = `wg${this.proxyId++}`

    const patch: AddOperation<T.NetworkInterfaceInfo>[] = [
      {
        op: PatchOp.ADD,
        path: `/serverInfo/network/gateways/${id}`,
        value: {
          name: params.name,
          public: params.public,
          secure: false,
          ipInfo: {
            name: id,
            scopeId: 3,
            deviceType: 'wireguard',
            subnets: ['192.168.1.10/24'],
            wanIp: '203.0.113.45',
            ntpServers: [],
            lanIp: ['192.168.1.10'],
            dnsServers: [],
          },
        },
      },
    ]
    this.mockRevision(patch)

    return { id }
  }

  async updateTunnel(params: RR.UpdateTunnelReq): Promise<RR.UpdateTunnelRes> {
    await pauseFor(2000)

    const patch: ReplaceOperation<string>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/serverInfo/network/gateways/${params.id}/label`,
        value: params.name,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async removeTunnel(params: RR.RemoveTunnelReq): Promise<RR.RemoveTunnelRes> {
    await pauseFor(2000)
    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/serverInfo/network/gateways/${params.id}`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  // wifi

  async enableWifi(params: RR.EnabledWifiReq): Promise<RR.EnabledWifiRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/network/wifi/enabled',
        value: params.enable,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async setWifiCountry(
    params: RR.SetWifiCountryReq,
  ): Promise<RR.SetWifiCountryRes> {
    await pauseFor(2000)
    return null
  }

  async getWifi(params: RR.GetWifiReq): Promise<RR.GetWifiRes> {
    await pauseFor(2000)
    return Mock.Wifi
  }

  async addWifi(params: RR.AddWifiReq): Promise<RR.AddWifiRes> {
    await pauseFor(2000)
    return null
  }

  async connectWifi(params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes> {
    await pauseFor(2000)
    return null
  }

  async deleteWifi(params: RR.DeleteWifiReq): Promise<RR.DeleteWifiRes> {
    await pauseFor(2000)
    return null
  }

  // smtp

  async setSmtp(params: RR.SetSMTPReq): Promise<RR.SetSMTPRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/smtp',
        value: params,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async clearSmtp(params: RR.ClearSMTPReq): Promise<RR.ClearSMTPRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/smtp',
        value: null,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async testSmtp(params: RR.TestSMTPReq): Promise<RR.TestSMTPRes> {
    await pauseFor(2000)
    return null
  }

  // ssh

  async getSshKeys(params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes> {
    await pauseFor(2000)
    return Mock.SshKeys
  }

  async addSshKey(params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes> {
    await pauseFor(2000)
    return Mock.SshKey
  }

  async deleteSshKey(params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes> {
    await pauseFor(2000)
    return null
  }

  // backup

  async getBackupTargets(
    params: RR.GetBackupTargetsReq,
  ): Promise<RR.GetBackupTargetsRes> {
    await pauseFor(2000)
    return Mock.BackupTargets
  }

  async addBackupTarget(
    params: RR.AddBackupTargetReq,
  ): Promise<RR.AddBackupTargetRes> {
    await pauseFor(2000)
    const { hostname, path, username } = params
    return {
      latfgvwdbhjsndmk: {
        type: 'cifs',
        hostname,
        path: path.replace(/\\/g, '/'),
        username,
        mountable: true,
        startOs: {},
      },
    }
  }

  async updateBackupTarget(
    params: RR.UpdateBackupTargetReq,
  ): Promise<RR.UpdateBackupTargetRes> {
    await pauseFor(2000)
    const { id, hostname, path, username } = params
    return {
      [id]: {
        ...(Mock.BackupTargets[id] as CifsBackupTarget),
        hostname,
        path,
        username,
      },
    }
  }

  async removeBackupTarget(
    params: RR.RemoveBackupTargetReq,
  ): Promise<RR.RemoveBackupTargetRes> {
    await pauseFor(2000)
    return null
  }

  async getBackupInfo(
    params: RR.GetBackupInfoReq,
  ): Promise<RR.GetBackupInfoRes> {
    await pauseFor(2000)
    return Mock.BackupInfo
  }

  async createBackup(params: RR.CreateBackupReq): Promise<RR.CreateBackupRes> {
    await pauseFor(2000)
    const serverPath = '/serverInfo/statusInfo/backupProgress'
    const ids = params.packageIds

    setTimeout(async () => {
      for (let i = 0; i < ids.length; i++) {
        const id = ids[i]
        const appPath = `/packageData/${id}/statusInfo/desired/main`
        const appPatch: ReplaceOperation<T.DesiredStatus['main']>[] = [
          {
            op: PatchOp.REPLACE,
            path: appPath,
            value: 'backing-up',
          },
        ]
        this.mockRevision(appPatch)

        await pauseFor(8000)

        this.mockRevision([
          {
            ...appPatch[0]!,
            value: 'stopped',
          },
        ])

        const serverPatch: ReplaceOperation<T.BackupProgress['complete']>[] = [
          {
            op: PatchOp.REPLACE,
            path: `${serverPath}/${id}/complete`,
            value: true,
          },
        ]
        this.mockRevision(serverPatch)
      }

      await pauseFor(1000)

      // remove backupProgress
      const lastPatch: ReplaceOperation<T.ServerStatus['backupProgress']>[] = [
        {
          op: PatchOp.REPLACE,
          path: serverPath,
          value: null,
        },
      ]
      this.mockRevision(lastPatch)
    }, 500)

    const originalPatch: ReplaceOperation<T.ServerStatus['backupProgress']>[] =
      [
        {
          op: PatchOp.REPLACE,
          path: serverPath,
          value: ids.reduce((acc, val) => {
            return {
              ...acc,
              [val]: { complete: false },
            }
          }, {}),
        },
      ]

    this.mockRevision(originalPatch)

    return null
  }

  // async addBackupTarget(
  //   type: BackupTargetType,
  //   params:
  //     | RR.AddCifsBackupTargetReq
  //     | RR.AddCloudBackupTargetReq
  //     | RR.AddDiskBackupTargetReq,
  // ): Promise<RR.AddBackupTargetRes> {
  //   await pauseFor(2000)
  //   const { path, name } = params
  //   return {
  //     latfgvwdbhjsndmk: {
  //       name,
  //       type: 'cifs',
  //       hostname: 'mockhotname',
  //       path: path.replace(/\\/g, '/'),
  //       username: 'mockusername',
  //       mountable: true,
  //       startOs: {},
  //     },
  //   }
  // }

  // async updateBackupTarget(
  //   type: BackupTargetType,
  //   params: RR.UpdateCifsBackupTargetReq | RR.UpdateCloudBackupTargetReq,
  // ): Promise<RR.UpdateBackupTargetRes> {
  //   await pauseFor(2000)
  //   return { [params.id]: Mock.BackupTargets.saved[params.id] }
  // }

  // async removeBackupTarget(
  //   params: RR.RemoveBackupTargetReq,
  // ): Promise<RR.RemoveBackupTargetRes> {
  //   await pauseFor(2000)
  //   return null
  // }

  // async getBackupJobs(
  //   params: RR.GetBackupJobsReq,
  // ): Promise<RR.GetBackupJobsRes> {
  //   await pauseFor(2000)
  //   return Mock.BackupJobs
  // }

  // async createBackupJob(
  //   params: RR.CreateBackupJobReq,
  // ): Promise<RR.CreateBackupJobRes> {
  //   await pauseFor(2000)
  //   return {
  //     id: 'hjdfbjsahdbn',
  //     name: params.name,
  //     targetId: Object.keys(Mock.BackupTargets.saved)[0],
  //     cron: params.cron,
  //     packageIds: params.packageIds,
  //   }
  // }

  // async updateBackupJob(
  //   params: RR.UpdateBackupJobReq,
  // ): Promise<RR.UpdateBackupJobRes> {
  //   await pauseFor(2000)
  //   return {
  //     ...Mock.BackupJobs[0],
  //     ...params,
  //   }
  // }

  // async deleteBackupJob(
  //   params: RR.DeleteBackupJobReq,
  // ): Promise<RR.DeleteBackupJobRes> {
  //   await pauseFor(2000)
  //   return null
  // }

  // async getBackupRuns(
  //   params: RR.GetBackupRunsReq,
  // ): Promise<RR.GetBackupRunsRes> {
  //   await pauseFor(2000)
  //   return Mock.BackupRuns
  // }

  // async deleteBackupRuns(
  //   params: RR.DeleteBackupRunsReq,
  // ): Promise<RR.DeleteBackupRunsRes> {
  //   await pauseFor(2000)
  //   return null
  // }

  // package

  async getPackageLogs(
    params: RR.GetPackageLogsReq,
  ): Promise<RR.GetPackageLogsRes> {
    await pauseFor(2000)
    let entries
    if (Math.random() < 0.2) {
      entries = Mock.ServerLogs
    } else {
      const arrLength = params.limit
        ? Math.ceil(params.limit / Mock.ServerLogs.length)
        : 10
      entries = new Array(arrLength)
        .fill(Mock.ServerLogs)
        .reduce((acc, val) => acc.concat(val), [])
    }
    return {
      entries,
      startCursor: 'startCursor',
      endCursor: 'end-cursor',
    }
  }

  async followPackageLogs(
    params: RR.FollowPackageLogsReq,
  ): Promise<RR.FollowPackageLogsRes> {
    await pauseFor(2000)
    return {
      startCursor: 'start-cursor',
      guid: 'logs-guid',
    }
  }

  async installPackage(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes> {
    await pauseFor(2000)

    setTimeout(async () => {
      this.installProgress(params.id)
    }, 1000)

    const patch: AddOperation<
      PackageDataEntry<InstallingState | UpdatingState>
    >[] = [
      {
        op: PatchOp.ADD,
        path: `/packageData/${params.id}`,
        value: {
          ...Mock.LocalPkgs[params.id]!,
          stateInfo: {
            // if installing
            state: 'installing',

            // if updating
            // state: 'updating',
            // manifest: mockPatchData.packageData[params.id]?.stateInfo.manifest!,

            // both
            installingInfo: {
              newManifest: Mock.LocalPkgs[params.id]?.stateInfo.manifest!,
              progress: PROGRESS,
            },
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async cancelInstallPackage(
    params: RR.CancelInstallPackageReq,
  ): Promise<RR.CancelInstallPackageRes> {
    await pauseFor(500)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/packageData/${params.id}`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async getActionInput(
    params: RR.GetActionInputReq,
  ): Promise<RR.GetActionInputRes> {
    await pauseFor(2000)
    return {
      eventId: 'ANZXNWIFRTTBZ6T52KQPZILIQQODDHXQ',
      value: Mock.MockConfig,
      spec: await Mock.getActionInputSpec(),
    }
  }

  async runAction(params: RR.ActionReq): Promise<RR.ActionRes> {
    await pauseFor(2000)

    const patch: ReplaceOperation<{ [key: string]: T.TaskEntry }>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${params.packageId}/tasks`,
        value: {},
      },
    ]
    this.mockRevision(patch)

    // return Mock.ActionResGroup
    return Mock.ActionResMessage
    // return Mock.ActionResSingle
  }

  async clearTask(params: RR.ClearTaskReq): Promise<RR.ClearTaskRes> {
    await pauseFor(2000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/packageData/${params.packageId}/tasks/${params.replayId}`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async restorePackages(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes> {
    await pauseFor(2000)
    const patch: AddOperation<PackageDataEntry>[] = params.ids.map(id => {
      setTimeout(async () => {
        this.installProgress(id)
      }, 2000)

      return {
        op: PatchOp.ADD,
        path: `/packageData/${id}`,
        value: {
          ...Mock.LocalPkgs[id]!,
          stateInfo: {
            state: 'restoring',
            installingInfo: {
              newManifest: Mock.LocalPkgs[id]?.stateInfo.manifest!,
              progress: PROGRESS,
            },
          },
        },
      }
    })

    this.mockRevision(patch)

    return null
  }

  async startPackage(params: RR.StartPackageReq): Promise<RR.StartPackageRes> {
    const path = `/packageData/${params.id}/statusInfo`

    await pauseFor(2000)

    setTimeout(async () => {
      const patch2: ReplaceOperation<T.StatusInfo>[] = [
        {
          op: PatchOp.REPLACE,
          path,
          value: {
            error: null,
            desired: { main: 'running' },
            started: new Date().toISOString(),
            health: {
              'ephemeral-health-check': {
                name: 'Ephemeral Health Check',
                result: 'success',
                message: null,
              },
              'unnecessary-health-check': {
                name: 'Unnecessary Health Check',
                result: 'disabled',
                message: 'Custom disabled message',
              },
              'chain-state': {
                name: 'Chain State',
                result: 'loading',
                message: 'Bitcoin is syncing from genesis',
              },
              'p2p-interface': {
                name: 'P2P Interface',
                result: 'waiting',
                message: 'Chain State',
              },
              'rpc-interface': {
                name: 'RPC Interface',
                result: 'failure',
                message: 'Custom failure message',
              },
            },
          },
        },
      ]
      this.mockRevision(patch2)
    }, 2000)

    const originalPatch: ReplaceOperation<T.StatusInfo>[] = [
      {
        op: PatchOp.REPLACE,
        path,
        value: {
          desired: { main: 'running' },
          started: null,
          error: null,
          health: {},
        },
      },
    ]

    this.mockRevision(originalPatch)

    return null
  }

  async restartPackage(
    params: RR.RestartPackageReq,
  ): Promise<RR.RestartPackageRes> {
    await pauseFor(2000)
    const path = `/packageData/${params.id}/statusInfo`

    setTimeout(async () => {
      const patch2: ReplaceOperation<T.StatusInfo>[] = [
        {
          op: PatchOp.REPLACE,
          path,
          value: {
            desired: { main: 'running' },
            error: null,
            started: new Date().toISOString(),
            health: {
              'ephemeral-health-check': {
                name: 'Ephemeral Health Check',
                result: 'starting',
                message: null,
              },
              'unnecessary-health-check': {
                name: 'Unnecessary Health Check',
                result: 'disabled',
                message: 'Custom disabled message',
              },
              'chain-state': {
                name: 'Chain State',
                result: 'loading',
                message: 'Bitcoin is syncing from genesis',
              },
              'p2p-interface': {
                name: 'P2P Interface',
                result: 'success',
                message: null,
              },
              'rpc-interface': {
                name: 'RPC Interface',
                result: 'failure',
                message: 'Custom failure message',
              },
            },
          },
        },
      ]
      this.mockRevision(patch2)
    }, this.revertTime)

    const patch: ReplaceOperation<T.StatusInfo>[] = [
      {
        op: PatchOp.REPLACE,
        path,
        value: {
          desired: { main: 'restarting' },
          started: null,
          error: null,
          health: {},
        },
      },
    ]

    this.mockRevision(patch)

    return null
  }

  async stopPackage(params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    await pauseFor(2000)
    const path = `/packageData/${params.id}/statusInfo`

    setTimeout(() => {
      const patch2: ReplaceOperation<T.StatusInfo>[] = [
        {
          op: PatchOp.REPLACE,
          path: path,
          value: {
            desired: { main: 'stopped' },
            error: null,
            health: {},
            started: null,
          },
        },
      ]
      this.mockRevision(patch2)
    }, this.revertTime)

    const patch: ReplaceOperation<T.StatusInfo>[] = [
      {
        op: PatchOp.REPLACE,
        path: path,
        value: {
          desired: { main: 'stopped' },
          error: null,
          health: {},
          started: new Date().toISOString(),
        },
      },
    ]

    this.mockRevision(patch)

    return null
  }

  async rebuildPackage(
    params: RR.RebuildPackageReq,
  ): Promise<RR.RebuildPackageRes> {
    return this.restartPackage(params)
  }

  async uninstallPackage(
    params: RR.UninstallPackageReq,
  ): Promise<RR.UninstallPackageRes> {
    await pauseFor(2000)

    setTimeout(async () => {
      const patch2: RemoveOperation[] = [
        {
          op: PatchOp.REMOVE,
          path: `/packageData/${params.id}`,
        },
      ]
      this.mockRevision(patch2)
    }, this.revertTime)

    const patch: ReplaceOperation<T.PackageState['state']>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${params.id}/stateInfo/state`,
        value: 'removing',
      },
    ]

    this.mockRevision(patch)

    return null
  }

  async sideloadPackage(): Promise<RR.SideloadPackageRes> {
    await pauseFor(2000)
    return {
      upload: 'sideload-upload-guid', // no significance, randomly generated
      progress: 'sideload-progress-guid', // no significance, randomly generated
    }
  }

  // async setServiceOutboundProxy(
  //   params: RR.SetServiceOutboundTunnelReq,
  // ): Promise<RR.SetServiceOutboundTunnelRes> {
  //   await pauseFor(2000)
  //   const patch = [
  //     {
  //       op: PatchOp.REPLACE,
  //       path: `/packageData/${params.packageId}/outboundProxy`,
  //       value: params.proxy,
  //     },
  //   ]
  //   this.mockRevision(patch)

  //   return null
  // }

  async initAcme(params: RR.InitAcmeReq): Promise<RR.InitAcmeRes> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.ADD,
        path: `/serverInfo/acme`,
        value: {
          [toAuthorityUrl(params.provider)]: { contact: params.contact },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async removeAcme(params: RR.RemoveAcmeReq): Promise<RR.RemoveAcmeRes> {
    await pauseFor(2000)

    const regex = new RegExp('/', 'g')

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/serverInfo/acme/${params.provider.replace(regex, '~1')}`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async addTorKey(params: RR.AddTorKeyReq): Promise<RR.AddTorKeyRes> {
    await pauseFor(2000)
    return 'vanityabcdefghijklmnop.onion'
  }

  async generateTorKey(params: RR.GenerateTorKeyReq): Promise<RR.AddTorKeyRes> {
    await pauseFor(2000)
    return 'abcdefghijklmnopqrstuv.onion'
  }

  async serverBindingToggleGateway(
    params: RR.ServerBindingToggleGatewayReq,
  ): Promise<RR.ServerBindingToggleGatewayRes> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: `/serverInfo/network/host/bindings/${params.internalPort}/net/publicEnabled`,
        value: params.enabled ? [params.gateway] : [],
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async serverAddOnion(params: RR.ServerAddOnionReq): Promise<RR.AddOnionRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.ADD,
        path: `/serverInfo/host/onions/0`,
        value: params.onion,
      },
      {
        op: PatchOp.ADD,
        path: `/serverInfo/host/hostnameInfo/80/0`,
        value: {
          kind: 'onion',
          hostname: {
            port: 80,
            sslPort: 443,
            value: params.onion,
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async serverRemoveOnion(
    params: RR.ServerRemoveOnionReq,
  ): Promise<RR.RemoveOnionRes> {
    await pauseFor(2000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/serverInfo/host/onions/0`,
      },
      {
        op: PatchOp.REMOVE,
        path: `/serverInfo/host/hostnameInfo/80/-1`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async osUiAddPublicDomain(
    params: RR.OsUiAddPublicDomainReq,
  ): Promise<RR.OsUiAddPublicDomainRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.ADD,
        path: `/serverInfo/host/publicDomains`,
        value: {
          [params.fqdn]: { gateway: params.gateway, acme: params.acme },
        },
      },
      {
        op: PatchOp.ADD,
        path: `/serverInfo/host/hostnameInfo/80/0`,
        value: {
          kind: 'ip',
          gatewayId: 'eth0',
          public: true,
          hostname: {
            kind: 'domain',
            domain: params.fqdn,
            subdomain: null,
            port: null,
            sslPort: 443,
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async osUiRemovePublicDomain(
    params: RR.OsUiRemovePublicDomainReq,
  ): Promise<RR.OsUiRemovePublicDomainRes> {
    await pauseFor(2000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/serverInfo/host/publicDomains/${params.fqdn}`,
      },
      {
        op: PatchOp.REMOVE,
        path: `/serverInfo/host/hostnameInfo/80/0`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async osUiAddPrivateDomain(
    params: RR.OsUiAddPrivateDomainReq,
  ): Promise<RR.OsUiAddPrivateDomainRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/serverInfo/host/privateDomains`,
        value: [params.fqdn],
      },
      {
        op: PatchOp.ADD,
        path: `/serverInfo/host/hostnameInfo/80/0`,
        value: {
          kind: 'ip',
          gatewayId: 'eth0',
          public: false,
          hostname: {
            kind: 'domain',
            domain: params.fqdn,
            subdomain: null,
            port: null,
            sslPort: 443,
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async osUiRemovePrivateDomain(
    params: RR.OsUiRemovePrivateDomainReq,
  ): Promise<RR.OsUiRemovePrivateDomainRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/serverInfo/host/privateDomains`,
        value: [],
      },
      {
        op: PatchOp.REMOVE,
        path: `/serverInfo/host/hostnameInfo/80/0`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async pkgBindingToggleGateway(
    params: RR.PkgBindingToggleGatewayReq,
  ): Promise<RR.PkgBindingToggleGatewayRes> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${params.package}/hosts/${params.host}/bindings/${params.internalPort}/net/privateDisabled`,
        value: params.enabled ? [] : [params.gateway],
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async pkgAddOnion(params: RR.PkgAddOnionReq): Promise<RR.AddOnionRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.ADD,
        path: `/packageData/${params.package}/hosts/${params.host}/onions/0`,
        value: params.onion,
      },
      {
        op: PatchOp.ADD,
        path: `/packageData/${params.package}/hosts/${params.host}/hostnameInfo/80/0`,
        value: {
          kind: 'onion',
          hostname: {
            port: 80,
            sslPort: 443,
            value: params.onion,
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async pkgRemoveOnion(
    params: RR.PkgRemoveOnionReq,
  ): Promise<RR.RemoveOnionRes> {
    await pauseFor(2000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/packageData/${params.package}/hosts/${params.host}/onions/0`,
      },
      {
        op: PatchOp.REMOVE,
        path: `/packageData/${params.package}/hosts/${params.host}/hostnameInfo/80/0`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async pkgAddPublicDomain(
    params: RR.PkgAddPublicDomainReq,
  ): Promise<RR.PkgAddPublicDomainRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.ADD,
        path: `/packageData/${params.package}/hosts/${params.host}/publicDomains`,
        value: {
          [params.fqdn]: { gateway: params.gateway, acme: params.acme },
        },
      },
      {
        op: PatchOp.ADD,
        path: `/packageData/${params.package}/hosts/${params.host}/hostnameInfo/80/0`,
        value: {
          kind: 'ip',
          gatewayId: 'eth0',
          public: true,
          hostname: {
            kind: 'domain',
            domain: params.fqdn,
            subdomain: null,
            port: null,
            sslPort: 443,
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async pkgRemovePublicDomain(
    params: RR.PkgRemovePublicDomainReq,
  ): Promise<RR.PkgRemovePublicDomainRes> {
    await pauseFor(2000)

    const patch: RemoveOperation[] = [
      {
        op: PatchOp.REMOVE,
        path: `/packageData/${params.package}/hosts/${params.host}/publicDomains/${params.fqdn}`,
      },
      {
        op: PatchOp.REMOVE,
        path: `/packageData/${params.package}/hosts/${params.host}/hostnameInfo/80/0`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async pkgAddPrivateDomain(
    params: RR.PkgAddPrivateDomainReq,
  ): Promise<RR.PkgAddPrivateDomainRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${params.package}/hosts/${params.host}/privateDomains`,
        value: [params.fqdn],
      },
      {
        op: PatchOp.ADD,
        path: `/packageData/${params.package}/hosts/${params.host}/hostnameInfo/80/0`,
        value: {
          kind: 'ip',
          gatewayId: 'eth0',
          public: false,
          hostname: {
            kind: 'domain',
            domain: params.fqdn,
            subdomain: null,
            port: null,
            sslPort: 443,
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async pkgRemovePrivateDomain(
    params: RR.PkgRemovePrivateDomainReq,
  ): Promise<RR.PkgRemovePrivateDomainRes> {
    await pauseFor(2000)

    const patch: Operation<any>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${params.package}/hosts/${params.host}/privateDomains`,
        value: [],
      },
      {
        op: PatchOp.REMOVE,
        path: `/packageData/${params.package}/hosts/${params.host}/hostnameInfo/80/0`,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  private async initProgress(): Promise<T.FullProgress> {
    const progress = JSON.parse(JSON.stringify(PROGRESS))

    for (let [i, phase] of progress.phases.entries()) {
      if (
        !phase.progress ||
        typeof phase.progress !== 'object' ||
        !phase.progress.total
      ) {
        await pauseFor(2000)

        progress.phases[i]!.progress = true

        if (
          progress.overall &&
          typeof progress.overall === 'object' &&
          progress.overall.total
        ) {
          const step = progress.overall.total / progress.phases.length
          progress.overall.done += step
        }
      } else {
        const step = phase.progress.total / 4

        while (phase.progress.done < phase.progress.total) {
          await pauseFor(200)

          phase.progress.done += step

          if (
            progress.overall &&
            typeof progress.overall === 'object' &&
            progress.overall.total
          ) {
            const step = progress.overall.total / progress.phases.length / 4

            progress.overall.done += step
          }

          if (phase.progress.done === phase.progress.total) {
            await pauseFor(250)

            progress.phases[i]!.progress = true
          }
        }
      }
    }
    return progress
  }

  private async installProgress(id: string): Promise<void> {
    const progress = JSON.parse(JSON.stringify(PROGRESS))

    for (let [i, phase] of progress.phases.entries()) {
      if (!phase.progress || phase.progress === true || !phase.progress.total) {
        await pauseFor(2000)

        const patches: Operation<any>[] = [
          {
            op: PatchOp.REPLACE,
            path: `/packageData/${id}/stateInfo/installingInfo/progress/phases/${i}/progress`,
            value: true,
          },
        ]

        // overall
        if (
          progress.overall &&
          typeof progress.overall === 'object' &&
          progress.overall.total
        ) {
          const step = progress.overall.total / progress.phases.length

          progress.overall.done += step

          patches.push({
            op: PatchOp.REPLACE,
            path: `/packageData/${id}/stateInfo/installingInfo/progress/overall/done`,
            value: progress.overall.done,
          })
        }

        this.mockRevision(patches)
      } else {
        const step = phase.progress.total / 4

        while (phase.progress.done < phase.progress.total) {
          await pauseFor(500)

          phase.progress.done += step

          const patches: Operation<any>[] = [
            {
              op: PatchOp.REPLACE,
              path: `/packageData/${id}/stateInfo/installingInfo/progress/phases/${i}/progress/done`,
              value: phase.progress.done,
            },
          ]

          // overall
          if (
            progress.overall &&
            typeof progress.overall === 'object' &&
            progress.overall.total
          ) {
            const step = progress.overall.total / progress.phases.length / 4

            progress.overall.done += step

            patches.push({
              op: PatchOp.REPLACE,
              path: `/packageData/${id}/stateInfo/installingInfo/progress/overall/done`,
              value: progress.overall.done,
            })
          }

          this.mockRevision(patches)

          if (phase.progress.done === phase.progress.total) {
            await pauseFor(250)
            this.mockRevision([
              {
                op: PatchOp.REPLACE,
                path: `/packageData/${id}/stateInfo/installingInfo/progress/phases/${i}/progress`,
                value: true,
              },
            ])
          }
        }
      }
    }

    await pauseFor(1000)
    this.mockRevision([
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${id}/stateInfo/installingInfo/progress/overall`,
        value: true,
      },
    ])

    await pauseFor(1000)
    const patch2: Operation<StateInfo>[] = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${id}/stateInfo`,
        value: {
          state: 'installed',
          manifest: Mock.LocalPkgs[id]?.stateInfo.manifest!,
        },
      },
    ]
    this.mockRevision(patch2)
  }

  private async updateOSProgress() {
    let size = 10000
    let downloaded = 0

    const patch0 = [
      {
        op: PatchOp.REPLACE,
        path: `/serverInfo/statusInfo/updateProgress/size`,
        value: size,
      },
    ]
    this.mockRevision(patch0)

    while (downloaded < size) {
      await pauseFor(250)
      downloaded += 500
      const patch = [
        {
          op: PatchOp.REPLACE,
          path: `/serverInfo/statusInfo/updateProgress/downloaded`,
          value: downloaded,
        },
      ]
      this.mockRevision(patch)
    }

    const patch2 = [
      {
        op: PatchOp.REPLACE,
        path: `/serverInfo/statusInfo/updateProgress/downloaded`,
        value: size,
      },
    ]
    this.mockRevision(patch2)

    setTimeout(async () => {
      const patch3: Operation<boolean>[] = [
        {
          op: PatchOp.REPLACE,
          path: '/serverInfo/statusInfo/updated',
          value: true,
        },
        {
          op: PatchOp.REMOVE,
          path: '/serverInfo/statusInfo/updateProgress',
        },
      ]
      this.mockRevision(patch3)
      // quickly revert server to "running" for continued testing
      await pauseFor(100)
      const patch4 = [
        {
          op: PatchOp.REPLACE,
          path: '/serverInfo/status',
          value: 'running',
        },
      ]
      this.mockRevision(patch4)
      // set patch indicating update is complete
      await pauseFor(100)
      const patch6 = [
        {
          op: PatchOp.REPLACE,
          path: '/serverInfo/statusInfo',
          value: Mock.ServerUpdated,
        },
      ]
      this.mockRevision(patch6)
    }, 1000)
  }

  private async mockRevision<T>(patch: Operation<T>[]): Promise<void> {
    const revision = {
      id: ++this.sequence,
      patch,
    }
    this.mockWsSource$.next(revision)
  }
}
