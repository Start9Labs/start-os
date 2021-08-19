import { Injectable } from '@angular/core'
import { pauseFor } from '../../util/misc.util'
import { ApiService } from './embassy-api.service'
import { PatchOp } from 'patch-db-client'
import { InstallProgress, PackageDataEntry, PackageMainStatus, PackageState, ServerStatus } from 'src/app/services/patch-db/data-model'
import { RR, WithRevision } from './api.types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { Mock } from './api.fixures'
import { HttpService } from '../http.service'
import markdown from 'raw-loader!src/assets/markdown/md-sample.md'

@Injectable()
export class MockApiService extends ApiService {
  private readonly revertTime = 4000

  constructor (
    private readonly http: HttpService,
  ) { super() }

  async getStatic (url: string): Promise<string> {
    await pauseFor(2000)
    return markdown
  }

  // db

  async getRevisions (since: number): Promise<RR.GetRevisionsRes> {
    return this.http.rpcRequest<RR.GetRevisionsRes>({ method: 'db.revisions', params: { since } })
  }

  async getDump (): Promise<RR.GetDumpRes> {
    return this.http.rpcRequest<RR.GetDumpRes>({ method: 'db.dump' })
  }

  async setDbValueRaw (params: RR.SetDBValueReq): Promise<RR.SetDBValueRes> {
    await pauseFor(2000)
    return this.http.rpcRequest<WithRevision<null>>({ method: 'db.put.ui', params })
  }

  // auth

  async login (params: RR.LoginReq): Promise<RR.loginRes> {
    await pauseFor(2000)
    return null
  }

  async logout (params: RR.LogoutReq): Promise<RR.LogoutRes> {
    await pauseFor(2000)
    return null
  }

  async getSessions (params: RR.GetSessionsReq): Promise<RR.GetSessionsRes> {
    await pauseFor(2000)
    return Mock.Sessions
  }

  async killSessions (params: RR.KillSessionsReq): Promise<RR.KillSessionsRes> {
    await pauseFor(2000)
    return null
  }

  // server

  async setShareStatsRaw (params: RR.SetShareStatsReq): Promise<RR.SetShareStatsRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/share-stats',
        value: params.value,
      },
    ]
    return this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
  }

  async getServerLogs (params: RR.GetServerLogsReq): Promise<RR.GetServerLogsRes> {
    await pauseFor(2000)
    return Mock.ServerLogs
  }

  async getServerMetrics (params: RR.GetServerMetricsReq): Promise<RR.GetServerMetricsRes> {
    await pauseFor(2000)
    return Mock.getServerMetrics()
  }

  async getPkgMetrics (params: RR.GetServerMetricsReq): Promise<RR.GetPackageMetricsRes> {
    await pauseFor(2000)
    return Mock.getAppMetrics()
  }

  async updateServerRaw (params: RR.UpdateServerReq): Promise<RR.UpdateServerRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/status',
        value: ServerStatus.Updating,
      },
    ]
    const res = await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    setTimeout(async () => {
      const patch = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/status',
          value: ServerStatus.Running,
        },
        {
          op: PatchOp.REPLACE,
          path: '/server-info/version',
          value: '3.1.0',
        },
      ]
      await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
      // quickly revert patch to proper version to prevent infinite refresh loop
      const patch2 = [
        {
          op: PatchOp.REPLACE,
          path: '/server-info/version',
          value: require('../../../../package.json').version,
        },
      ]
      this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch: patch2 } })
    }, this.revertTime)

    return res
  }

  async restartServer (params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    await pauseFor(2000)
    return null
  }

  async shutdownServer (params: RR.ShutdownServerReq): Promise<RR.ShutdownServerRes> {
    await pauseFor(2000)
    return null
  }

  // marketplace URLs

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    await pauseFor(2000)
    return Mock.MarketplaceEos
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    await pauseFor(2000)
    return {
      categories: ['featured', 'bitcoin', 'lightning', 'data', 'messaging', 'social', 'alt coin'],
    }
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    await pauseFor(2000)
    return Mock.MarketplacePkgsList
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    await pauseFor(2000)
    return Mock.ReleaseNotes
  }

  async getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes> {
    await pauseFor(2000)
    return params.ids.reduce((obj, id) => {
      obj[id] = '1.3.0'
      return obj
    }, { })
  }

  // async setPackageMarketplaceRaw (params: RR.SetPackageMarketplaceReq): Promise<RR.SetPackageMarketplaceRes> {
  //   await pauseFor(2000)
  //   const patch = [
  //     {
  //       op: PatchOp.REPLACE,
  //       path: '/server-info/package-marketplace',
  //       value: params.url,
  //     },
  //   ]
  //   return this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
  // }

  // password
  // async updatePassword (params: RR.UpdatePasswordReq): Promise<RR.UpdatePasswordRes> {
  //   await pauseFor(2000)
  //   return null
  // }

  // notification

  async getNotificationsRaw (params: RR.GetNotificationsReq): Promise<RR.GetNotificationsRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: '/server-info/unread-notification-count',
        value: 0,
      },
    ]
    const { revision } = await this.http.rpcRequest<WithRevision<RR.GetNotificationsRes>>({ method: 'db.patch', params: { patch } }) as WithRevision<null>
    return {
      response: Mock.Notifications,
      revision,
    }
  }

  async deleteNotification (params: RR.DeleteNotificationReq): Promise<RR.DeleteNotificationRes> {
    await pauseFor(2000)
    return null
  }

  async deleteAllNotifications (params: RR.DeleteAllNotificationsReq): Promise<RR.DeleteAllNotificationsRes> {
    await pauseFor(2000)
    return null
  }

  // wifi

  async getWifi (params: RR.GetWifiReq): Promise < RR.GetWifiRes > {
    await pauseFor(2000)
    return Mock.Wifi
  }

  async setWifiCountry (params: RR.SetWifiCountryReq): Promise <RR.SetWifiCountryRes> {
    await pauseFor(2000)
    return null
  }

  async addWifi (params: RR.AddWifiReq): Promise<RR.AddWifiRes> {
    await pauseFor(2000)
    return null
  }

  async connectWifi (params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes> {
    await pauseFor(2000)
    return null
  }

  async deleteWifi (params: RR.DeleteWifiReq): Promise<RR.DeleteWifiRes> {
    await pauseFor(2000)
    return null
  }

  // ssh

  async getSshKeys (params: RR.GetSSHKeysReq): Promise<RR.GetSSHKeysRes> {
    await pauseFor(2000)
    return Mock.SshKeys
  }

  async addSshKey (params: RR.AddSSHKeyReq): Promise<RR.AddSSHKeyRes> {
    await pauseFor(2000)
    return Mock.SshKey
  }

  async deleteSshKey (params: RR.DeleteSSHKeyReq): Promise<RR.DeleteSSHKeyRes> {
    await pauseFor(2000)
    return null
  }

  // backup

  async createBackupRaw (params: RR.CreateBackupReq): Promise<RR.CreateBackupRes> {
    await pauseFor(2000)
    const path = '/server-info/status'
    const patch = [
      {
        op: PatchOp.REPLACE,
        path,
        value: ServerStatus.BackingUp,
      },
    ]
    const res = await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    setTimeout(() => {
      const patch = [
        {
          op: PatchOp.REPLACE,
          path,
          value: ServerStatus.Running,
        },
      ]
      this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    }, this.revertTime)
    return res
  }

  async restoreBackupRaw (params: RR.RestoreBackupReq): Promise<RR.RestoreBackupRes> {
    await pauseFor(2000)
    return null
  }

  // disk

  async getDisks (params: RR.GetDisksReq): Promise<RR.GetDisksRes> {
    await pauseFor(2000)
    return Mock.Disks
  }

  async ejectDisk (params: RR.EjectDisksReq): Promise<RR.EjectDisksRes> {
    await pauseFor(2000)
    return null
  }

  // package

  async getPackageProperties (params: RR.GetPackagePropertiesReq): Promise<RR.GetPackagePropertiesRes<any>['data']> {
    await pauseFor(2000)
    return parsePropertiesPermissive(Mock.PackageProperties)
  }

  async getPackageLogs (params: RR.GetPackageLogsReq): Promise<RR.GetPackageLogsRes> {
    await pauseFor(2000)
    return Mock.PackageLogs
  }

  async installPackageRaw (params: RR.InstallPackageReq): Promise<RR.InstallPackageRes> {
    await pauseFor(2000)
    const initialProgress: InstallProgress = {
      size: 120,
      downloaded: 0,
      'download-complete': false,
      validated: 0,
      'validation-complete': false,
      unpacked: 0,
      'unpack-complete': false,
    }
    const pkg: PackageDataEntry = {
      ...Mock[params.id],
      state: PackageState.Installing,
      'install-progress': initialProgress,
    }
    const patch = [
      {
        op: PatchOp.ADD,
        path: `/package-data/${params.id}`,
        value: pkg,
      },
    ]
    const res = await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    setTimeout(async () => {
      this.updateProgress(params.id, initialProgress)
    }, 1000)
    return res
  }

  async dryUpdatePackage (params: RR.DryUpdatePackageReq): Promise<RR.DryUpdatePackageRes> {
    await pauseFor(2000)
    return { }
  }

  async getPackageConfig (params: RR.GetPackageConfigReq): Promise<RR.GetPackageConfigRes> {
    await pauseFor(2000)
    return Mock.PackageConfig
  }

  async drySetPackageConfig (params: RR.DrySetPackageConfigReq): Promise<RR.DrySetPackageConfigRes> {
    await pauseFor(2000)
    return { }
  }

  async setPackageConfigRaw (params: RR.SetPackageConfigReq): Promise<RR.SetPackageConfigRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: `/package-data/${params.id}/installed/status/configured`,
        value: true,
      },
    ]
    return this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
  }

  async restorePackageRaw (params: RR.RestorePackageReq): Promise<RR.RestorePackageRes> {
    await pauseFor(2000)
    const path = `/package-data/${params.id}/installed/status/main/status`
    const patch = [
      {
        op: PatchOp.REPLACE,
        path,
        value: PackageMainStatus.Restoring,
      },
    ]
    const res = await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    setTimeout(() => {
      const patch = [
        {
          op: PatchOp.REPLACE,
          path,
          value: PackageMainStatus.Stopped,
        },
      ]
      this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    }, this.revertTime)
    return res
  }

  async executePackageAction (params: RR.ExecutePackageActionReq): Promise<RR.ExecutePackageActionRes> {
    await pauseFor(2000)
    return {
      message: 'Action success!',
      value: 'new password',
      copyable: true,
      qr: false,
    }
  }

  async startPackageRaw (params: RR.StartPackageReq): Promise<RR.StartPackageRes> {
    await pauseFor(2000)
    const path = `/package-data/${params.id}/installed/status/main/status`
    const patch = [
      {
        op: PatchOp.REPLACE,
        path,
        value: PackageMainStatus.Running,
      },
    ]
    return this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
  }

  async dryStopPackage (params: RR.DryStopPackageReq): Promise<RR.DryStopPackageRes> {
    await pauseFor(2000)
    return { }
  }

  async stopPackageRaw (params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    await pauseFor(2000)
    const path = `/package-data/${params.id}/installed/status/main/status`
    const patch = [
      {
        op: PatchOp.REPLACE,
        path,
        value: PackageMainStatus.Stopping,
      },
    ]
    const res = await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    setTimeout(() => {
      const patch = [
        {
          op: PatchOp.REPLACE,
          path,
          value: PackageMainStatus.Stopped,
        },
      ]
      this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    }, this.revertTime)

    return res
  }

  async dryRemovePackage (params: RR.DryRemovePackageReq): Promise<RR.DryRemovePackageRes> {
    await pauseFor(2000)
    return { }
  }

  async removePackageRaw (params: RR.RemovePackageReq): Promise<RR.RemovePackageRes> {
    await pauseFor(2000)
    const patch = [
      {
        op: PatchOp.REPLACE,
        path: `/package-data/${params.id}/state`,
        value: PackageState.Removing,
      },
    ]

    const res = await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    setTimeout(async () => {
      const patch = [
        {
          op: PatchOp.REMOVE,
          path: `/package-data/${params.id}`,
        },
      ]
      this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    }, this.revertTime)

    return res
  }

  async dryConfigureDependency (params: RR.DryConfigureDependencyReq): Promise<RR.DryConfigureDependencyRes> {
    await pauseFor(2000)
    return {
      testnet: true,
      // objectList: [],
      // unionList: [],
      randomEnum: 'option2',
      favoriteNumber: 9,
      secondaryNumbers: [2, 3, 5, 6],
      rpcsettings: {
        laws: {
          law1: 'The 1st law',
          law2: 'The 2nd law',
        },
        rpcpass: null,
        rpcuser: '123',
        rulemakers: [],
      },
      advanced: {
        notifications: ['call', 'text'],
      },
      // bitcoinNode: undefined,
      port: 22,
      // rpcallowip: undefined,
      // rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
    }
  }

  private async updateProgress (id: string, initialProgress: InstallProgress) {
    const phases = [
      { progress: 'downloaded', completion: 'download-complete'},
      { progress: 'validated', completion: 'validation-complete'},
      { progress: 'unpacked', completion: 'unpack-complete'},
    ]
    for (let phase of phases) {
      let i = initialProgress[phase.progress]
      while (i < initialProgress.size) {
        await pauseFor(1000)
        i = Math.min(i + 5, initialProgress.size)
        initialProgress[phase.progress] = i
        if (i === initialProgress.size) {
          initialProgress[phase.completion] = true
        }
        const patch = [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${id}/install-progress`,
            value: initialProgress,
          },
        ]
        await this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
      }
    }

    setTimeout(() => {
      const patch = [
        {
          op: PatchOp.REPLACE,
          path: `/package-data/${id}/state`,
          value: PackageState.Installed,
        },
      ]
      this.http.rpcRequest<WithRevision<null>>({ method: 'db.patch', params: { patch } })
    }, 1000)
  }
}
