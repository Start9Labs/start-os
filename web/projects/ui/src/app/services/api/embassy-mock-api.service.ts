import { Injectable } from '@angular/core'
import { Log, pauseFor } from '@start9labs/shared'
import { ApiService } from './embassy-api.service'
import {
  Operation,
  PatchOp,
  pathFromArray,
  RemoveOperation,
  Update,
} from 'patch-db-client'
import {
  DataModel,
  FullProgress,
  InstallingState,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
  ServerStatus,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { CifsBackupTarget, RR } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { Mock } from './api.fixures'
import markdown from 'raw-loader!../../../../../shared/assets/markdown/md-sample.md'
import {
  EMPTY,
  iif,
  interval,
  map,
  Observable,
  shareReplay,
  Subject,
  switchMap,
  tap,
  timer,
} from 'rxjs'
import { LocalStorageBootstrap } from '../patch-db/local-storage-bootstrap'
import { mockPatchData } from './mock-patch'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import { AuthService } from '../auth.service'
import { ConnectionService } from '../connection.service'
import { StoreInfo } from '@start9labs/marketplace'

const PROGRESS: FullProgress = {
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
  readonly mockWsSource$ = new Subject<Update<DataModel>>()
  private readonly revertTime = 1800
  sequence = 0

  constructor(
    private readonly bootstrapper: LocalStorageBootstrap,
    private readonly connectionService: ConnectionService,
    private readonly auth: AuthService,
  ) {
    super()
    this.auth.isVerified$
      .pipe(
        tap(() => {
          this.sequence = 0
        }),
        switchMap(verified =>
          iif(
            () => verified,
            timer(2000).pipe(
              tap(() => {
                this.connectionService.websocketConnected$.next(true)
              }),
            ),
            EMPTY,
          ),
        ),
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

  // db

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

    setTimeout(() => {
      this.mockWsSource$.next({ id: 1, value: mockPatchData })
    }, 2000)

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

  // server

  async echo(params: RR.EchoReq, url?: string): Promise<RR.EchoRes> {
    if (url) {
      const num = Math.floor(Math.random() * 10) + 1
      if (num > 8) return params.message
      throw new Error()
    }
    await pauseFor(2000)
    return params.message
  }

  openPatchWebsocket$(): Observable<Update<DataModel>> {
    return this.mockWsSource$.pipe(
      shareReplay({ bufferSize: 1, refCount: true }),
    )
  }

  openLogsWebsocket$(config: WebSocketSubjectConfig<Log>): Observable<Log> {
    return interval(50).pipe(
      map((_, index) => {
        // mock fire open observer
        if (index === 0) config.openObserver?.next(new Event(''))
        if (index === 100) throw new Error('HAAHHA')
        return Mock.ServerLogs[0]
      }),
    )
  }

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
      'start-cursor': 'startCursor',
      'end-cursor': 'endCursor',
    }
  }

  async getKernelLogs(
    params: RR.GetServerLogsReq,
  ): Promise<RR.GetServerLogsRes> {
    await pauseFor(2000)
    const entries = this.randomLogs(params.limit)

    return {
      entries,
      'start-cursor': 'startCursor',
      'end-cursor': 'endCursor',
    }
  }

  async getTorLogs(params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes> {
    await pauseFor(2000)
    const entries = this.randomLogs(params.limit)

    return {
      entries,
      'start-cursor': 'startCursor',
      'end-cursor': 'endCursor',
    }
  }

  async followServerLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    await pauseFor(2000)
    return {
      'start-cursor': 'start-cursor',
      guid: '7251d5be-645f-4362-a51b-3a85be92b31e',
    }
  }

  async followKernelLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    await pauseFor(2000)
    return {
      'start-cursor': 'start-cursor',
      guid: '7251d5be-645f-4362-a51b-3a85be92b31e',
    }
  }

  async followTorLogs(
    params: RR.FollowServerLogsReq,
  ): Promise<RR.FollowServerLogsRes> {
    await pauseFor(2000)
    return {
      'start-cursor': 'start-cursor',
      guid: '7251d5be-645f-4362-a51b-3a85be92b31e',
    }
  }

  randomLogs(limit = 1): Log[] {
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
        path: '/server-info/status-info/update-progress',
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
        path: '/server-info/status-info/restarting',
        value: true,
      },
    ]
    this.mockRevision(patch)

    setTimeout(() => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/status-info/restarting',
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
        path: '/server-info/status-info/shutting-down',
        value: true,
      },
    ]
    this.mockRevision(patch)

    setTimeout(() => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/status-info/shutting-down',
          value: false,
        },
      ]
      this.mockRevision(patch2)
    }, 2000)

    return null
  }

  async systemRebuild(
    params: RR.SystemRebuildReq,
  ): Promise<RR.SystemRebuildRes> {
    return this.restartServer(params)
  }

  async repairDisk(params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    await pauseFor(2000)
    return null
  }

  async resetTor(params: RR.ResetTorReq): Promise<RR.ResetTorRes> {
    await pauseFor(2000)
    return null
  }

  async toggleZram(params: RR.ToggleZramReq): Promise<RR.ToggleZramRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/zram',
        value: params.enable,
      },
    ]
    this.mockRevision(patch)

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

  async getEos(): Promise<RR.GetMarketplaceEosRes> {
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
        path: '/server-info/unread-notification-count',
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
        'embassy-os': null,
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
    const path = '/server-info/status-info/backup-progress'
    const ids = params['package-ids']

    setTimeout(async () => {
      for (let i = 0; i < ids.length; i++) {
        const id = ids[i]
        const appPath = `/package-data/${id}/installed/status/main/status`
        const appPatch = [
          {
            op: PatchOp.REPLACE,
            path: appPath,
            value: PackageMainStatus.BackingUp,
          },
        ]
        this.mockRevision(appPatch)

        await pauseFor(8000)

        this.mockRevision([
          {
            ...appPatch[0],
            value: PackageMainStatus.Stopped,
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
      entries = Mock.PackageLogs
    } else {
      const arrLength = params.limit
        ? Math.ceil(params.limit / Mock.PackageLogs.length)
        : 10
      entries = new Array(arrLength)
        .fill(Mock.PackageLogs)
        .reduce((acc, val) => acc.concat(val), [])
    }
    return {
      entries,
      'start-cursor': 'startCursor',
      'end-cursor': 'endCursor',
    }
  }

  async followPackageLogs(
    params: RR.FollowPackageLogsReq,
  ): Promise<RR.FollowPackageLogsRes> {
    await pauseFor(2000)
    return {
      'start-cursor': 'start-cursor',
      guid: '7251d5be-645f-4362-a51b-3a85be92b31e',
    }
  }

  async installPackage(
    params: RR.InstallPackageReq,
  ): Promise<RR.InstallPackageRes> {
    await pauseFor(2000)

    setTimeout(async () => {
      this.updateProgress(params.id)
    }, 1000)

    const manifest = Mock.LocalPkgs[params.id]['state-info'].manifest

    const patch: Operation<InstallingState | UpdatingState>[] = [
      {
        op: PatchOp.ADD,
        path: `/package-data/${params.id}/state-info`,
        value: {
          // if installing
          state: PackageState.Installing,

          // if updating
          // state: PackageState.Updating,
          // manifest,

          // both
          'installing-info': {
            'new-manifest': manifest,
            progress: PROGRESS,
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
        path: `/package-data/${params.id}/installed/status/configured`,
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
        this.updateProgress(id)
      }, 2000)

      return {
        op: PatchOp.ADD,
        path: `/package-data/${id}`,
        value: {
          ...Mock.LocalPkgs[id],
          'state-info': {
            state: PackageState.Restoring,
            'installing-info': {
              'new-manifest': Mock.LocalPkgs[id]['state-info'].manifest!,
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
    const path = `/package-data/${params.id}/installed/status/main`

    await pauseFor(2000)

    setTimeout(async () => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: path + '/status',
          value: PackageMainStatus.Running,
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
        value: PackageMainStatus.Starting,
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
    const path = `/package-data/${params.id}/installed/status/main`

    setTimeout(async () => {
      const patch2: Operation<any>[] = [
        {
          op: PatchOp.REPLACE,
          path: path + '/status',
          value: PackageMainStatus.Starting,
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
          value: PackageMainStatus.Running,
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
        value: PackageMainStatus.Restarting,
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
    const path = `/package-data/${params.id}/installed/status/main`

    setTimeout(() => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: path,
          value: {
            status: PackageMainStatus.Stopped,
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
          status: PackageMainStatus.Stopping,
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
          path: `/package-data/${params.id}`,
        },
      ]
      this.mockRevision(patch2)
    }, this.revertTime)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: `/package-data/${params.id}/state-info/state`,
        value: PackageState.Removing,
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
      'old-config': Mock.MockConfig,
      'new-config': Mock.MockDependencyConfig,
      spec: Mock.ConfigSpec,
    }
  }

  async sideloadPackage(
    params: RR.SideloadPackageReq,
  ): Promise<RR.SideloadPacakgeRes> {
    await pauseFor(2000)
    return '4120e092-05ab-4de2-9fbd-c3f1f4b1df9e' // no significance, randomly generated
  }

  private async updateProgress(id: string): Promise<void> {
    const progress = { ...PROGRESS }

    for (let phase of progress.phases) {
      if (typeof phase.progress !== 'object' || !phase.progress.total) continue

      while (phase.progress.done < phase.progress.total) {
        await pauseFor(250)
        // i = Math.min(i + 5, size)
        // progress[phase.progress] = i

        // if (i === progress.size) {
        //   progress[phase.completion] = true
        // }

        const patch = [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${id}/state-info/installing-info/progress`,
            value: { ...progress },
          },
        ]
        this.mockRevision(patch)
      }
    }

    setTimeout(() => {
      const patch2: Operation<any>[] = [
        {
          op: PatchOp.REPLACE,
          path: `/package-data/${id}/state-info/state`,
          value: PackageState.Installed,
        },
        {
          op: PatchOp.REMOVE,
          path: `/package-data/${id}/state-info/installing-info`,
        },
        {
          op: PatchOp.REMOVE,
          path: `/package-data/${id}/state-info/new-manifest`,
        },
      ]
      this.mockRevision(patch2)
    }, 1000)
  }

  private async updateOSProgress() {
    let size = 10000
    let downloaded = 0

    const patch0 = [
      {
        op: PatchOp.REPLACE,
        path: `/server-info/status-info/update-progress/size`,
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
          path: `/server-info/status-info/update-progress/downloaded`,
          value: downloaded,
        },
      ]
      this.mockRevision(patch)
    }

    const patch2 = [
      {
        op: PatchOp.REPLACE,
        path: `/server-info/status-info/update-progress/downloaded`,
        value: size,
      },
    ]
    this.mockRevision(patch2)

    setTimeout(async () => {
      const patch3: Operation<ServerStatus>[] = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/status',
          value: ServerStatus.Updated,
        },
        {
          op: PatchOp.REMOVE,
          path: '/server-info/status-info/update-progress',
        },
      ]
      this.mockRevision(patch3)
      // quickly revert server to "running" for continued testing
      await pauseFor(100)
      const patch4 = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/status',
          value: ServerStatus.Running,
        },
      ]
      this.mockRevision(patch4)
      // set patch indicating update is complete
      await pauseFor(100)
      const patch6 = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/status-info',
          value: Mock.ServerUpdated,
        },
      ]
      this.mockRevision(patch6)
    }, 1000)
  }

  private async mockRevision<T>(patch: Operation<T>[]): Promise<void> {
    if (!this.sequence) {
      const { sequence } = this.bootstrapper.init()
      this.sequence = sequence
    }
    const revision = {
      id: ++this.sequence,
      patch,
    }
    this.mockWsSource$.next(revision)
  }
}
