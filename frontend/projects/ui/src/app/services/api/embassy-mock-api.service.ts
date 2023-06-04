import { Injectable } from '@angular/core'
import { pauseFor, Log, getSetupStatusMock } from '@start9labs/shared'
import { ApiService } from './embassy-api.service'
import {
  PatchOp,
  Update,
  Operation,
  RemoveOperation,
  pathFromArray,
} from 'patch-db-client'
import {
  DataModel,
  DependencyErrorType,
  InstallProgress,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { BackupTargetType, Metrics, RR } from './api.types'
import { Mock } from './api.fixures'
import markdown from 'raw-loader!../../../../../shared/assets/markdown/md-sample.md'
import {
  EMPTY,
  iif,
  interval,
  map,
  Observable,
  ReplaySubject,
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

const PROGRESS: InstallProgress = {
  size: 120,
  downloaded: 0,
  'download-complete': false,
  validated: 0,
  'validation-complete': false,
  unpacked: 0,
  'unpack-complete': false,
}

@Injectable()
export class MockApiService extends ApiService {
  readonly mockWsSource$ = new ReplaySubject<Update<DataModel>>()
  private readonly revertTime = 2000
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
          this.patchStream$.next([])
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

  async uploadPackage(guid: string, body: Blob): Promise<void> {
    await pauseFor(2000)
  }

  async uploadFile(body: Blob): Promise<string> {
    await pauseFor(2000)
    return 'returnedhash'
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
    return this.withRevision(patch)
  }

  // auth

  async getPubKey() {
    await pauseFor(1000)

    // randomly generated
    // const keystore = jose.JWK.createKeyStore()
    // this.pubkey = await keystore.generate('EC', 'P-256')

    // generated from backend
    this.pubkey = await this.jose.then(jose =>
      jose.JWK.asKey({
        kty: 'EC',
        crv: 'P-256',
        x: 'yHTDYSfjU809fkSv9MmN4wuojf5c3cnD7ZDN13n-jz4',
        y: '8Mpkn744A5KDag0DmX2YivB63srjbugYZzWc3JOpQXI',
      }),
    )
  }

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

  // server

  async echo(params: RR.EchoReq): Promise<RR.EchoRes> {
    await pauseFor(2000)
    return params.message
  }

  openPatchWebsocket$(): Observable<Update<DataModel>> {
    return this.mockWsSource$
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

  openMetricsWebsocket$(
    config: WebSocketSubjectConfig<Metrics>,
  ): Observable<Metrics> {
    return interval(2000).pipe(
      map((_, index) => {
        // mock fire open observer
        if (index === 0) config.openObserver?.next(new Event(''))
        if (index === 4) throw new Error('HAHAHA')
        return Mock.getMetrics()
      }),
    )
  }

  async getSystemTime(
    params: RR.GetSystemTimeReq,
  ): Promise<RR.GetSystemTimeRes> {
    await pauseFor(2000)
    return new Date().toUTCString()
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
    return {
      guid: 'iqudh37um-i38u3-34-a51b-jkhd783ein',
      metrics: Mock.getMetrics(),
    }
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
    return this.withRevision(patch, 'updating')
  }

  async setServerClearnetAddress(
    params: RR.SetServerClearnetAddressReq,
  ): Promise<RR.SetServerClearnetAddressRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/clearnetAddress',
        value: params.address,
      },
    ]
    return this.withRevision(patch, null)
  }

  async restartServer(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes> {
    await pauseFor(2000)
    return null
  }

  async shutdownServer(
    params: RR.ShutdownServerReq,
  ): Promise<RR.ShutdownServerRes> {
    await pauseFor(2000)
    return null
  }

  async systemRebuild(
    params: RR.RestartServerReq,
  ): Promise<RR.RestartServerRes> {
    await pauseFor(2000)
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

  async toggleZram(params: RR.ToggleZramReq): Promise<RR.ToggleZramRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/zram',
        value: params.enable,
      },
    ]
    return this.withRevision(patch, null)
  }

  // marketplace URLs

  async marketplaceProxy(
    path: string,
    params: Record<string, string>,
    url: string,
    arch = '',
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

  // password
  // async updatePassword (params: RR.UpdatePasswordReq): Promise<RR.UpdatePasswordRes> {
  //   await pauseFor(2000)
  //   return null
  // }

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
    return this.withRevision(patch, Mock.Notifications)
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

  // domains

  async claimStart9MeDomain(
    params: RR.ClaimStart9MeReq,
  ): Promise<RR.ClaimStart9MeRes> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/start9MeSubdomain',
        value: {
          value: 'xyz',
          createdAt: new Date(),
        },
      },
    ]
    return this.withRevision(patch, null)
  }

  async deleteStart9MeDomain(
    params: RR.DeleteStart9MeReq,
  ): Promise<RR.DeleteStart9MeRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/start9MeSubdomain',
        value: null,
      },
    ]
    return this.withRevision(patch, null)
  }

  async addDomain(params: RR.AddDomainReq): Promise<RR.AddDomainRes> {
    await pauseFor(2000)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/domains',
        value: [
          {
            value: params.domain,
            provider: params.provider,
            createdAt: new Date(),
          },
        ],
      },
    ]
    return this.withRevision(patch, null)
  }

  async deleteDomain(params: RR.DeleteDomainReq): Promise<RR.DeleteDomainRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/domains',
        value: [],
      },
    ]
    return this.withRevision(patch, null)
  }

  // wifi

  async enableWifi(params: RR.EnableWifiReq): Promise<RR.EnableWifiRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/wifi-enabled',
        value: params.enable,
      },
    ]
    return this.withRevision(patch, null)
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

  // email

  async testEmail(params: RR.TestEmailReq): Promise<RR.TestEmailRes> {
    await pauseFor(2000)
    return null
  }

  async configureEmail(
    params: RR.ConfigureEmailReq,
  ): Promise<RR.ConfigureEmailRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/email',
        value: params,
      },
    ]

    return this.withRevision(patch)
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
    type: BackupTargetType,
    params:
      | RR.AddCifsBackupTargetReq
      | RR.AddCloudBackupTargetReq
      | RR.AddDiskBackupTargetReq,
  ): Promise<RR.AddBackupTargetRes> {
    await pauseFor(2000)
    const { path, name } = params
    return {
      id: 'latfgvwdbhjsndmk',
      name,
      type: 'cifs',
      hostname: 'mockhotname',
      path: path.replace(/\\/g, '/'),
      username: 'mockusername',
      mountable: true,
      'embassy-os': null,
    }
  }

  async updateBackupTarget(
    type: BackupTargetType,
    params: RR.UpdateCifsBackupTargetReq | RR.UpdateCloudBackupTargetReq,
  ): Promise<RR.UpdateBackupTargetRes> {
    await pauseFor(2000)
    return Mock.BackupTargets.saved.find(b => b.id === params.id)!
  }

  async removeBackupTarget(
    params: RR.RemoveBackupTargetReq,
  ): Promise<RR.RemoveBackupTargetRes> {
    await pauseFor(2000)
    return null
  }

  async getBackupJobs(
    params: RR.GetBackupJobsReq,
  ): Promise<RR.GetBackupJobsRes> {
    await pauseFor(2000)
    return Mock.BackupJobs
  }

  async createBackupJob(
    params: RR.CreateBackupJobReq,
  ): Promise<RR.CreateBackupJobRes> {
    await pauseFor(2000)
    return {
      id: 'hjdfbjsahdbn',
      name: params.name,
      target: Mock.BackupTargets.saved[0],
      cron: params.cron,
      'package-ids': params['package-ids'],
    }
  }

  async updateBackupJob(
    params: RR.UpdateBackupJobReq,
  ): Promise<RR.UpdateBackupJobRes> {
    await pauseFor(2000)
    return {
      ...Mock.BackupJobs[0],
      ...params,
    }
  }

  async deleteBackupJob(
    params: RR.DeleteBackupJobReq,
  ): Promise<RR.DeleteBackupJobRes> {
    await pauseFor(2000)
    return null
  }

  async getBackupRuns(
    params: RR.GetBackupRunsReq,
  ): Promise<RR.GetBackupRunsRes> {
    await pauseFor(2000)
    return Mock.BackupRuns
  }

  async deleteBackupRuns(
    params: RR.DeleteBackupRunsReq,
  ): Promise<RR.DeleteBackupRunsRes> {
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

    return this.withRevision(originalPatch)
  }

  // package

  async getPackageCredentials(
    params: RR.GetPackageCredentialsReq,
  ): Promise<RR.GetPackageCredentialsRes> {
    await pauseFor(2000)
    return {
      password: 'specialPassword$',
    }
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

    const patch: Operation<PackageDataEntry>[] = [
      {
        op: PatchOp.ADD,
        path: `/package-data/${params.id}`,
        value: {
          ...Mock.LocalPkgs[params.id],
          // state: PackageState.Installing,
          state: PackageState.Updating,
          'install-progress': { ...PROGRESS },
        },
      },
    ]
    return this.withRevision(patch)
  }

  async dryUpdatePackage(
    params: RR.DryUpdatePackageReq,
  ): Promise<RR.DryUpdatePackageRes> {
    await pauseFor(2000)
    return {
      lnd: {
        dependency: 'bitcoind',
        error: {
          type: DependencyErrorType.IncorrectVersion,
          expected: '>0.23.0',
          received: params.version,
        },
      },
    }
  }

  async getPackageConfig(
    params: RR.GetPackageConfigReq,
  ): Promise<RR.GetPackageConfigRes> {
    await pauseFor(2000)
    return {
      config: Mock.MockConfig,
      spec: await Mock.getInputSpec(),
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
    return this.withRevision(patch)
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
          state: PackageState.Restoring,
          'install-progress': { ...PROGRESS },
          installed: undefined,
        },
      }
    })

    return this.withRevision(patch)
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
      if (params.id !== 'bitcoind') {
        const patch2 = [
          {
            op: PatchOp.REPLACE,
            path: path + '/health',
            value: {},
          },
        ]
        this.mockRevision(patch2)
      }

      const patch3 = [
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
      this.mockRevision(patch3)
    }, 2000)

    const originalPatch = [
      {
        op: PatchOp.REPLACE,
        path: path + '/status',
        value: PackageMainStatus.Starting,
      },
    ]

    return this.withRevision(originalPatch)
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

    return this.withRevision(patch)
  }

  async stopPackage(params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    await pauseFor(2000)
    const path = `/package-data/${params.id}/installed/status/main`

    setTimeout(() => {
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: path + '/status',
          value: PackageMainStatus.Stopped,
        },
      ]
      this.mockRevision(patch2)
    }, this.revertTime)

    const patch = [
      {
        op: PatchOp.REPLACE,
        path: path + '/status',
        value: PackageMainStatus.Stopping,
      },
    ]

    return this.withRevision(patch)
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
        path: `/package-data/${params.id}/state`,
        value: PackageState.Removing,
      },
    ]

    return this.withRevision(patch)
  }

  async dryConfigureDependency(
    params: RR.DryConfigureDependencyReq,
  ): Promise<RR.DryConfigureDependencyRes> {
    await pauseFor(2000)
    return {
      'old-config': Mock.MockConfig,
      'new-config': Mock.MockDependencyConfig,
      spec: await Mock.getInputSpec(),
    }
  }

  async sideloadPackage(
    params: RR.SideloadPackageReq,
  ): Promise<RR.SideloadPacakgeRes> {
    await pauseFor(2000)
    return '4120e092-05ab-4de2-9fbd-c3f1f4b1df9e' // no significance, randomly generated
  }

  async getSetupStatus() {
    return getSetupStatusMock()
  }

  async followLogs(): Promise<string> {
    await pauseFor(1000)
    return 'fake-guid'
  }

  private async updateProgress(id: string): Promise<void> {
    const progress = { ...PROGRESS }
    const phases = [
      { progress: 'downloaded', completion: 'download-complete' },
      { progress: 'validated', completion: 'validation-complete' },
      { progress: 'unpacked', completion: 'unpack-complete' },
    ] as const

    for (let phase of phases) {
      let i = progress[phase.progress]
      const size = progress?.size || 0
      while (i < size) {
        await pauseFor(250)
        i = Math.min(i + 5, size)
        progress[phase.progress] = i

        if (i === progress.size) {
          progress[phase.completion] = true
        }

        const patch = [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${id}/install-progress`,
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
          path: `/package-data/${id}/state`,
          value: PackageState.Installed,
        },
        {
          op: PatchOp.ADD,
          path: `/package-data/${id}/installed`,
          value: { ...Mock.LocalPkgs[id].installed },
        },
        {
          op: PatchOp.REMOVE,
          path: `/package-data/${id}/install-progress`,
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
      const patch3: Operation<boolean>[] = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/status-info/updated',
          value: true,
        },
        {
          op: PatchOp.REMOVE,
          path: '/server-info/status-info/update-progress',
        },
      ]
      this.mockRevision(patch3)
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

  private async withRevision<T>(
    patch: Operation<unknown>[],
    response: T | null = null,
  ): Promise<T> {
    if (!this.sequence) {
      const { sequence } = this.bootstrapper.init()
      this.sequence = sequence
    }

    this.patchStream$.next([
      {
        id: ++this.sequence,
        patch,
      },
    ])

    return response as T
  }
}
