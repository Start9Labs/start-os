import { Injectable } from '@angular/core'
import { Log, RPCErrorDetails, pauseFor } from '@start9labs/shared'
import { ApiService } from './embassy-api.service'
import {
  Operation,
  PatchOp,
  pathFromArray,
  RemoveOperation,
  Revision,
} from 'patch-db-client'
import {
  InstallingState,
  PackageDataEntry,
  StateInfo,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { CifsBackupTarget, RR } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { Mock } from './api.fixures'
import markdown from 'raw-loader!../../../../../shared/assets/markdown/md-sample.md'
import {
  from,
  interval,
  map,
  Observable,
  shareReplay,
  startWith,
  Subject,
  tap,
} from 'rxjs'
import { mockPatchData } from './mock-patch'
import { AuthService } from '../auth.service'
import { StoreInfo } from '@start9labs/marketplace'
import { T } from '@start9labs/start-sdk'

const PROGRESS: T.FullProgress = {
  overall: {
    done: 0,
    total: 120,
  },
  phases: [
    {
      name: 'Downloading',
      progress: {
        done: 0,
        total: 40,
      },
    },
    {
      name: 'Validating',
      progress: {
        done: 0,
        total: 40,
      },
    },
    {
      name: 'Installing',
      progress: {
        done: 0,
        total: 40,
      },
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

  async getStatic(url: string): Promise<string> {
    await pauseFor(2000)
    return markdown
  }

  async uploadPackage(guid: string, body: Blob): Promise<string> {
    await pauseFor(2000)
    return 'success'
  }

  // websocket

  openWebsocket$<T>(
    guid: string,
    config: RR.WebsocketConfig<T>,
  ): Observable<T> {
    if (guid === 'db-guid') {
      return this.mockWsSource$.pipe<any>(
        shareReplay({ bufferSize: 1, refCount: true }),
      )
    } else if (guid === 'logs-guid') {
      return interval(50).pipe<any>(
        map((_, index) => {
          // mock fire open observer
          if (index === 0) config.openObserver?.next(new Event(''))
          if (index === 100) throw new Error('HAAHHA')
          return Mock.ServerLogs[0]
        }),
      )
    } else if (guid === 'init-progress-guid') {
      return from(this.initProgress()).pipe(
        startWith(PROGRESS),
      ) as Observable<T>
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

    return this.stateIndex === 1 ? 'initializing' : 'running'
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

  async initGetProgress(): Promise<RR.InitGetProgressRes> {
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

  async getTorLogs(params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes> {
    await pauseFor(2000)
    const entries = this.randomLogs(params.limit)

    return {
      entries,
      startCursor: 'startCursor',
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

  async followKernelLogs(
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

  private randomLogs(limit = 1): Log[] {
    const arrLength = Math.ceil(limit / Mock.ServerLogs.length)
    const logs = new Array(arrLength)
      .fill(Mock.ServerLogs)
      .reduce((acc, val) => acc.concat(val), [])

    return logs
  }

  async getServerMetrics(
    params: RR.GetServerMetricsReq,
  ): Promise<RR.GetServerMetricsRes> {
    await pauseFor(2000)
    return Mock.getServerMetrics()
  }

  async getPkgMetrics(
    params: RR.GetServerMetricsReq,
  ): Promise<RR.GetPackageMetricsRes> {
    await pauseFor(2000)
    return Mock.getAppMetrics()
  }

  async updateServer(url?: string): Promise<RR.UpdateServerRes> {
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

  async resetTor(params: RR.ResetTorReq): Promise<RR.ResetTorRes> {
    await pauseFor(2000)
    return null
  }

  // marketplace URLs

  async marketplaceProxy(
    path: string,
    params: Record<string, string>,
    url: string,
  ): Promise<any> {
    await pauseFor(2000)

    if (path === '/package/v0/info') {
      const info: StoreInfo = {
        name: 'Start9 Registry',
        categories: [
          'bitcoin',
          'lightning',
          'data',
          'featured',
          'messaging',
          'social',
          'alt coin',
        ],
      }
      return info
    } else if (path === '/package/v0/index') {
      return Mock.MarketplacePkgsList
    } else if (path.startsWith('/package/v0/release-notes')) {
      return Mock.ReleaseNotes
    } else if (path.includes('instructions') || path.includes('license')) {
      return markdown
    }
  }

  async checkOSUpdate(qp: RR.CheckOSUpdateReq): Promise<RR.CheckOSUpdateRes> {
    await pauseFor(2000)
    return Mock.MarketplaceEos
  }

  // notification

  async getNotifications(
    params: RR.GetNotificationsReq,
  ): Promise<RR.GetNotificationsRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/serverInfo/unreadNotificationCount',
        value: 0,
      },
    ]
    this.mockRevision(patch)

    return Mock.Notifications
  }

  async deleteNotification(
    params: RR.DeleteNotificationReq,
  ): Promise<RR.DeleteNotificationRes> {
    await pauseFor(2000)
    return null
  }

  async deleteAllNotifications(
    params: RR.DeleteAllNotificationsReq,
  ): Promise<RR.DeleteAllNotificationsRes> {
    await pauseFor(2000)
    return null
  }

  // wifi

  async getWifi(params: RR.GetWifiReq): Promise<RR.GetWifiRes> {
    await pauseFor(2000)
    return Mock.Wifi
  }

  async setWifiCountry(
    params: RR.SetWifiCountryReq,
  ): Promise<RR.SetWifiCountryRes> {
    await pauseFor(2000)
    return null
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
        startOs: null,
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
    const path = '/serverInfo/statusInfo/backupProgress'
    const ids = params.packageIds

    setTimeout(async () => {
      for (let i = 0; i < ids.length; i++) {
        const id = ids[i]
        const appPath = `/packageData/${id}/status/main/status`
        const appPatch = [
          {
            op: PatchOp.REPLACE,
            path: appPath,
            value: 'backingUp',
          },
        ]
        this.mockRevision(appPatch)

        await pauseFor(8000)

        this.mockRevision([
          {
            ...appPatch[0],
            value: 'stopped',
          },
        ])
        this.mockRevision([
          {
            op: PatchOp.REPLACE,
            path: `${path}/${id}/complete`,
            value: true,
          },
        ])
      }

      await pauseFor(1000)

      // set server back to running
      const lastPatch = [
        {
          op: PatchOp.REPLACE,
          path,
          value: null,
        },
      ]
      this.mockRevision(lastPatch)
    }, 500)

    const originalPatch = [
      {
        op: PatchOp.REPLACE,
        path,
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

  // package

  async getPackageProperties(
    params: RR.GetPackagePropertiesReq,
  ): Promise<RR.GetPackagePropertiesRes<2>['data']> {
    await pauseFor(2000)
    return parsePropertiesPermissive(Mock.PackageProperties)
  }

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

    const patch: Operation<
      PackageDataEntry<InstallingState | UpdatingState>
    >[] = [
      {
        op: PatchOp.ADD,
        path: `/packageData/${params.id}`,
        value: {
          ...Mock.LocalPkgs[params.id],
          stateInfo: {
            // if installing
            // state: PackageState.Installing,

            // if updating
            state: 'updating',
            manifest: mockPatchData.packageData[params.id].stateInfo.manifest!,

            // both
            installingInfo: {
              newManifest: Mock.LocalPkgs[params.id].stateInfo.manifest,
              progress: PROGRESS,
            },
          },
        },
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async getPackageConfig(
    params: RR.GetPackageConfigReq,
  ): Promise<RR.GetPackageConfigRes> {
    await pauseFor(2000)
    return {
      config: Mock.MockConfig,
      spec: Mock.ConfigSpec,
    }
  }

  async drySetPackageConfig(
    params: RR.DrySetPackageConfigReq,
  ): Promise<RR.DrySetPackageConfigRes> {
    await pauseFor(2000)
    return {}
  }

  async setPackageConfig(
    params: RR.SetPackageConfigReq,
  ): Promise<RR.SetPackageConfigRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${params.id}/status/configured`,
        value: true,
      },
    ]
    this.mockRevision(patch)

    return null
  }

  async restorePackages(
    params: RR.RestorePackagesReq,
  ): Promise<RR.RestorePackagesRes> {
    await pauseFor(2000)
    const patch: Operation<PackageDataEntry>[] = params.ids.map(id => {
      setTimeout(async () => {
        this.installProgress(id)
      }, 2000)

      return {
        op: PatchOp.ADD,
        path: `/packageData/${id}`,
        value: {
          ...Mock.LocalPkgs[id],
          stateInfo: {
            state: 'restoring',
            installingInfo: {
              newManifest: Mock.LocalPkgs[id].stateInfo.manifest!,
              progress: PROGRESS,
            },
          },
        },
      }
    })

    this.mockRevision(patch)

    return null
  }

  async executePackageAction(
    params: RR.ExecutePackageActionReq,
  ): Promise<RR.ExecutePackageActionRes> {
    await pauseFor(2000)
    return Mock.ActionResponse
  }

  async startPackage(params: RR.StartPackageReq): Promise<RR.StartPackageRes> {
    const path = `/packageData/${params.id}/status/main`

    await pauseFor(2000)

    setTimeout(async () => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: path + '/status',
          value: 'running',
        },
        {
          op: PatchOp.REPLACE,
          path: path + '/started',
          value: new Date().toISOString(),
        },
      ]
      this.mockRevision(patch2)

      const patch3 = [
        {
          op: PatchOp.REPLACE,
          path: path + '/health',
          value: {
            'ephemeral-health-check': {
              result: 'starting',
            },
            'unnecessary-health-check': {
              result: 'disabled',
            },
          },
        },
      ]
      this.mockRevision(patch3)

      await pauseFor(2000)

      const patch4 = [
        {
          op: PatchOp.REPLACE,
          path: path + '/health',
          value: {
            'ephemeral-health-check': {
              result: 'starting',
            },
            'unnecessary-health-check': {
              result: 'disabled',
            },
            'chain-state': {
              result: 'loading',
              message: 'Bitcoin is syncing from genesis',
            },
            'p2p-interface': {
              result: 'success',
            },
            'rpc-interface': {
              result: 'failure',
              error: 'RPC interface unreachable.',
            },
          },
        },
      ]
      this.mockRevision(patch4)
    }, 2000)

    const originalPatch = [
      {
        op: PatchOp.REPLACE,
        path: path + '/status',
        value: 'starting',
      },
    ]

    this.mockRevision(originalPatch)

    return null
  }

  async restartPackage(
    params: RR.RestartPackageReq,
  ): Promise<RR.RestartPackageRes> {
    // first enact stop
    await pauseFor(2000)
    const path = `/packageData/${params.id}/status/main`

    setTimeout(async () => {
      const patch2: Operation<any>[] = [
        {
          op: PatchOp.REPLACE,
          path: path + '/status',
          value: 'starting',
        },
        {
          op: PatchOp.ADD,
          path: path + '/restarting',
          value: true,
        },
      ]
      this.mockRevision(patch2)

      await pauseFor(2000)

      const patch3: Operation<any>[] = [
        {
          op: PatchOp.REPLACE,
          path: path + '/status',
          value: 'running',
        },
        {
          op: PatchOp.REMOVE,
          path: path + '/restarting',
        },
        {
          op: PatchOp.REPLACE,
          path: path + '/health',
          value: {
            'ephemeral-health-check': {
              result: 'starting',
            },
            'unnecessary-health-check': {
              result: 'disabled',
            },
            'chain-state': {
              result: 'loading',
              message: 'Bitcoin is syncing from genesis',
            },
            'p2p-interface': {
              result: 'success',
            },
            'rpc-interface': {
              result: 'failure',
              error: 'RPC interface unreachable.',
            },
          },
        } as any,
      ]
      this.mockRevision(patch3)
    }, this.revertTime)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: path + '/status',
        value: 'restarting',
      },
      {
        op: PatchOp.REPLACE,
        path: path + '/health',
        value: {},
      },
    ]

    this.mockRevision(patch)

    return null
  }

  async stopPackage(params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    await pauseFor(2000)
    const path = `/packageData/${params.id}/status/main`

    setTimeout(() => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: path,
          value: {
            status: 'stopped',
          },
        },
      ]
      this.mockRevision(patch2)
    }, this.revertTime)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: path,
        value: {
          status: 'stopping',
          timeout: '35s',
        },
      },
    ]

    this.mockRevision(patch)

    return null
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

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: `/packageData/${params.id}/stateInfo/state`,
        value: 'removing',
      },
    ]

    this.mockRevision(patch)

    return null
  }

  async dryConfigureDependency(
    params: RR.DryConfigureDependencyReq,
  ): Promise<RR.DryConfigureDependencyRes> {
    await pauseFor(2000)
    return {
      oldConfig: Mock.MockConfig,
      newConfig: Mock.MockDependencyConfig,
      spec: Mock.ConfigSpec,
    }
  }

  async sideloadPackage(): Promise<RR.SideloadPackageRes> {
    await pauseFor(2000)
    return {
      upload: '4120e092-05ab-4de2-9fbd-c3f1f4b1df9e', // no significance, randomly generated
      progress: '5120e092-05ab-4de2-9fbd-c3f1f4b1df9e', // no significance, randomly generated
    }
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

        progress.phases[i].progress = true

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

            progress.phases[i].progress = true
          }
        }
      }
    }
    return progress
  }

  private async installProgress(id: string): Promise<void> {
    const progress = JSON.parse(JSON.stringify(PROGRESS))

    for (let [i, phase] of progress.phases.entries()) {
      if (
        !phase.progress ||
        typeof phase.progress !== 'object' ||
        !phase.progress.total
      ) {
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
          manifest: Mock.LocalPkgs[id].stateInfo.manifest,
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
