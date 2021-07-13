import { Injectable } from '@angular/core'
import { pauseFor } from '../../util/misc.util'
import { ApiService } from './api.service'
import { Observable } from 'rxjs'
import { PatchOp, Store, Update } from 'patch-db-client'
import { DataModel, PackageDataEntry, PackageMainStatus, PackageState, ServerStatus } from 'src/app/services/patch-db/data-model'
import { RR } from './api-types'
import { parsePropertiesPermissive } from 'src/app/util/properties.util'
import { Mock } from './mock-app-fixures'
import { HttpService, Method } from '../http.service'
import markdown from 'raw-loader!src/assets/markdown/md-sample.md'
import { ConfigService } from '../config.service'
const configService = new ConfigService()
@Injectable()
export class MockApiService extends ApiService {
  sequence = 0
  welcomeAck = false

  constructor (
    private readonly http: HttpService,
  ) { super() }

  // every time a patch is returned from the mock, we override its sequence to be 1 more than the last sequence in the patch-db as provided by `o`.
  watch$ (store: Store<DataModel>): Observable<Update<DataModel>> {
    store.sequence$.subscribe(seq => {
      console.log('INCOMING: ', seq)
      if (this.sequence < seq) {
        this.sequence = seq
      }
    })
    return super.watch$()
  }

  async getStatic (url: string): Promise<string> {
    await pauseFor(2000)
    return markdown
  }

  // db

  async getRevisions (since: number): Promise<RR.GetRevisionsRes> {
    // await pauseFor(2000)
    // return {
    //   ...Mock.DbDump,
    //   id: this.nextSequence(),
    // }
    return this.http.rpcRequest({ method: 'db.revisions', params: { since } })
  }

  async getDump (): Promise<RR.GetDumpRes> {
    // await pauseFor(2000)
    // return {
    //   ...Mock.DbDump,
    //   id: this.nextSequence(),
    // }
    return this.http.rpcRequest({ method: 'db.dump' })
  }

  async setDbValueRaw (params: RR.SetDBValueReq): Promise<RR.SetDBValueRes> {
    // await pauseFor(2000)
    // return {
    //   response: null,
    //   revision: {
    //     id: this.nextSequence(),
    //     patch: [
    //       {
    //         op: PatchOp.REPLACE,
    //         path: params.pointer,
    //         value: params.value,
    //       },
    //     ],
    //     expireId: null,
    //   },
    // }
    return this.http.rpcRequest({ method: 'db.put.ui', params })
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

  // server

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
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: '/server-info/status',
            value: ServerStatus.Updating,
          },
        ],
        expireId: null,
      },
    }
  }

  async restartServer (params: RR.RestartServerReq): Promise<RR.RestartServerRes> {
    await pauseFor(2000)
    return null
  }

  async shutdownServer (params: RR.ShutdownServerReq): Promise<RR.ShutdownServerRes> {
    await pauseFor(2000)
    return null
  }

  // network

  async refreshLan (params: RR.RefreshLanReq): Promise<RR.RefreshLanRes> {
    await pauseFor(2000)
    return null
  }

  // registry

  async setRegistryRaw (params: RR.SetRegistryReq): Promise<RR.SetRegistryRes> {
    await pauseFor(2000)
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: '/server-info/registry',
            value: params.url,
          },
        ],
        expireId: null,
      },
    }
  }

  // notification

  async getNotificationsRaw (params: RR.GetNotificationsReq): Promise<RR.GetNotificationsRes> {
    await pauseFor(2000)
    return {
      response: Mock.Notifications,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: '/server-info/unread-notification-count',
            value: 0,
          },
        ],
        expireId: null,
      },
    }
  }

  async deleteNotification (params: RR.DeleteNotificationReq): Promise<RR.DeleteNotificationRes> {
    await pauseFor(2000)
    return null
  }

  // wifi

  async addWifi (params: RR.AddWifiReq): Promise<RR.AddWifiRes> {
    await pauseFor(2000)
    return null
  }

  async connectWifiRaw (params: RR.ConnectWifiReq): Promise<RR.ConnectWifiRes> {
    await pauseFor(2000)
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: '/server-info/wifi/selected',
            value: params.ssid,
          },
          {
            op: PatchOp.REPLACE,
            path: '/server-info/wifi/connected',
            value: params.ssid,
          },
        ],
        expireId: null,
      },
    }
  }

  async deleteWifiRaw (params: RR.DeleteWifiReq): Promise<RR.DeleteWifiRes> {
    await pauseFor(2000)
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: '/server-info/wifi/selected',
            value: null,
          },
          {
            op: PatchOp.REPLACE,
            path: '/server-info/wifi/connected',
            value: null,
          },
        ],
        expireId: null,
      },
    }
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
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: '/server-info/status',
            value: ServerStatus.BackingUp,
          },
        ],
        expireId: null,
      },
    }
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
    const pkg: PackageDataEntry = {
      ...Mock.bitcoinproxy,
      state: PackageState.Installing,
      // installed: undefined,
      'install-progress': {
        size: 100,
        downloaded: 10,
        'download-complete': false,
        validated: 1,
        'validation-complete': true,
        read: 50,
        'read-complete': false,
      },
    }
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.ADD,
            path: `/package-data/${params.id}`,
            value: pkg,
          },
        ],
        expireId: null,
      },
    }
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
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${params.id}/installed/status/configured`,
            value: true,
          },
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${params.id}/installed/status/main/status`,
            value: PackageMainStatus.Running,
          },
        ],
        expireId: null,
      },
    }
  }

  async restorePackageRaw (params: RR.RestorePackageReq): Promise<RR.RestorePackageRes> {
    await pauseFor(2000)
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${params.id}/installed/status/main/status`,
            value: PackageMainStatus.Restoring,
          },
        ],
        expireId: null,
      },
    }
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
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${params.id}/installed/status/main/status`,
            value: PackageMainStatus.Running,
          },
        ],
        expireId: null,
      },
    }
  }

  async dryStopPackage (params: RR.DryStopPackageReq): Promise<RR.DryStopPackageRes> {
    await pauseFor(2000)
    return { }
  }

  async stopPackageRaw (params: RR.StopPackageReq): Promise<RR.StopPackageRes> {
    await pauseFor(2000)
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${params.id}/installed/status/main/status`,
            value: PackageMainStatus.Stopping,
          },
        ],
        expireId: null,
      },
    }
  }

  async dryRemovePackage (params: RR.DryRemovePackageReq): Promise<RR.DryRemovePackageRes> {
    await pauseFor(2000)
    return { }
  }

  async removePackageRaw (params: RR.RemovePackageReq): Promise<RR.RemovePackageRes> {
    await pauseFor(2000)
    return {
      response: null,
      revision: {
        id: this.nextSequence(),
        patch: [
          {
            op: PatchOp.REPLACE,
            path: `/package-data/${params.id}/state`,
            value: PackageState.Removing,
          },
        ],
        expireId: null,
      },
    }
  }

  async dryConfigureDependency (params: RR.DryConfigureDependencyReq): Promise<RR.DryConfigureDependencyRes> {
    await pauseFor(2000)
    return { }
  }

  // marketplace

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    const registryURL = configService.mocks.registryURL
    if (!registryURL) {
      await pauseFor(2000)
      return {
        categories: ['featured', 'bitcoin', 'lightning', 'data', 'messaging', 'social', 'alt coin'],
      }
    }
    const url = `${registryURL}/marketplace/data`
    let md = await this.http.simpleGet(url)
    return (md as any)
  }

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    const registryURL = configService.mocks.registryURL
    if (!registryURL) {
      await pauseFor(2000)
      return Mock.MarketplaceEos
    }
    const url = `${registryURL}/sys/version/eos`
    let eos = await this.http.simpleGet(url)
    return (eos as any)
    await pauseFor(2000)
    return Mock.MarketplaceEos
  }

  async getAvailableList (params: RR.GetAvailableListReq): Promise<RR.GetAvailableListRes> {
    const registryURL = configService.mocks.registryURL
    if (!registryURL) {
      await pauseFor(2000)
      return Mock.AvailableList
    }
    const url = `${registryURL}/marketplace/available/list?${params.category ? `category=${params.category}` : ''}&per-page=${params['per-page'] || '20'}&page=${params.page || '1'}&query=${params.query || ''}`
    let av = await this.http.simpleGet(url)
    return (av as any)
  }

  async getAvailableShow (params: RR.GetAvailableShowReq): Promise<RR.GetAvailableShowRes> {
    const registryURL = configService.mocks.registryURL
    if (!registryURL) {
      await pauseFor(2000)
      return Mock.AvailableShow[params.id][params.version || 'latest']
    }
    const url = `${registryURL}/marketplace/available?id=${params.id}&version=${params.version || '1.3.0'}`
    let res = await this.http.simpleGet(url)
    console.log('res RES RES', res)
    return (res as any)
  }

  private nextSequence () {
    console.log('next')
    this.sequence++
    return this.sequence
  }
}
