import { Injectable } from '@angular/core'
import { AppStatus, AppModel } from '../../models/app-model'
import { AppAvailablePreview, AppAvailableFull, AppInstalledPreview, AppInstalledFull, DependentBreakage, AppAvailableVersionSpecificInfo } from '../../models/app-types'
import { S9Notification, SSHFingerprint, ServerStatus, ServerModel, DiskInfo } from '../../models/server-model'
import { pauseFor } from '../../util/misc.util'
import { ApiService, ReqRes } from './api.service'
import { ApiServer, Unit as EmptyResponse, Unit } from './api-types'
import { AppMetrics, AppMetricsVersioned, parseMetricsPermissive } from 'src/app/util/metrics.util'
import { mockApiAppAvailableFull, mockApiAppAvailableVersionInfo, mockApiAppInstalledFull, mockAppDependentBreakages, toInstalledPreview } from './mock-app-fixures'

//@TODO consider moving to test folders.
@Injectable()
export class MockApiService extends ApiService {
  constructor (
    private readonly appModel: AppModel,
    private readonly serverModel: ServerModel,
  ) {
    super()
  }

  async postLogin () : Promise<Unit> {
    return {  }
  }

  async postLogout () : Promise<Unit> {
    return {  }
  }

  async postConfigureDependency (dependencyId: string, dependentId: string, dryRun?: boolean): Promise<{ config: object, breakages: DependentBreakage[] }> {
    await pauseFor(2000)
    throw new Error ('some misc backend error ohh we forgot to make this endpoint or something')
    // return { config: mockCupsDependentConfig, breakages: [ ] }
  }

  async getServer (): Promise<ApiServer> {
    return mockGetServer()
  }

  async getCheckAuth (): Promise<ReqRes.GetCheckAuthRes> {
    return { }
  }

  async getVersionLatest (): Promise<ReqRes.GetVersionLatestRes> {
    return mockGetVersionLatest()
  }

  async getServerMetrics (): Promise<ReqRes.GetServerMetricsRes> {
    return mockGetServerMetrics()
  }

  async getNotifications (page: number, perPage: number): Promise<S9Notification[]> {
    return mockGetNotifications()
  }

  async deleteNotification (id: string): Promise<EmptyResponse> {
    return mockDeleteNotification()
  }

  async getExternalDisks (): Promise<DiskInfo[]> {
    return mockGetExternalDisks()
  }

  async updateAgent (thing: any): Promise<EmptyResponse> {
    return mockPostUpdateAgent()
  }

  async getAvailableApps (): Promise<AppAvailablePreview[]> {
    return mockGetAvailableApps()
  }

  async getAvailableApp (appId: string): Promise<AppAvailableFull> {
    // throw new Error('Some horrible horrible error message gosh its awful')
    return mockGetAvailableApp(appId)
      .then(res => {
        return {
          ...res,
          versionViewing: res.versionLatest,
        }
      })
  }

  async getAvailableAppVersionSpecificInfo (appId: string, versionSpec: string): Promise<AppAvailableVersionSpecificInfo> {
    return mockGetAvailableAppVersionInfo()
  }

  async getInstalledApp (appId: string): Promise<AppInstalledFull> {
    return mockGetInstalledApp(appId)
  }

  async getAppMetrics (appId: string): Promise<AppMetrics> {
    return mockGetAppMetrics().then(parseMetricsPermissive)
  }

  async getInstalledApps (): Promise<AppInstalledPreview[]> {
    return mockGetInstalledApps()
  }

  async getAppConfig (appId: string): Promise<ReqRes.GetAppConfigRes> {
    return mockGetAppConfig()
  }

  async getAppLogs (appId: string, params: ReqRes.GetAppLogsReq = { }): Promise<string[]> {
    return mockGetAppLogs()
  }

  async installApp (appId: string, version: string, dryRun: boolean): Promise<AppInstalledFull & { breakages: DependentBreakage[] }> {
    return mockInstallApp(appId)
  }

  async uninstallApp (appId: string, dryRun: boolean): Promise<{ breakages: DependentBreakage[] }> {
    return mockUninstallApp()
  }

  async startApp (appId: string): Promise<EmptyResponse> {
    console.log('start app mock')
    await mockStartApp()
    this.appModel.update({ id: appId, status: AppStatus.RUNNING })
    return { }
  }

  async stopApp (appId: string, dryRun = false): Promise<{ breakages: DependentBreakage[] }> {
    await mockStopApp()
    if (!dryRun) this.appModel.update({ id: appId, status: AppStatus.STOPPED })
    return mockAppDependentBreakages
  }

  async restartApp (appId: string): Promise<Unit> {
    return { }
  }

  async createAppBackup (appId: string, logicalname: string, password = ''): Promise<EmptyResponse> {
    await mockCreateAppBackup()
    this.appModel.update({ id: appId, status: AppStatus.CREATING_BACKUP })
    return { }
  }

  async stopAppBackup (appId: string): Promise<EmptyResponse> {
    await mockStopAppBackup()
    this.appModel.update({ id: appId, status: AppStatus.STOPPED })
    return { }
  }

  async restoreAppBackup (appId: string, logicalname: string, password?: string): Promise<EmptyResponse> {
    await mockCreateAppBackup()
    this.appModel.update({ id: appId, status: AppStatus.RESTORING_BACKUP })
    return { }
  }

  async patchAppConfig (app: AppInstalledPreview, config: object, dryRun?: boolean): Promise<{ breakages: DependentBreakage[] }> {
    return mockPatchAppConfig()
  }

  async patchServerConfig (attr: string, value: any): Promise<EmptyResponse> {
    await mockPatchServerConfig()
    this.serverModel.update({ [attr]: value })
    return { }
  }

  async wipeAppData (app: AppInstalledPreview): Promise<EmptyResponse> {
    return mockWipeAppData()
  }

  async addSSHKey (sshKey: string): Promise<EmptyResponse> {
    const fingerprint = await mockAddSSHKey()
    this.serverModel.update({ ssh: [...this.serverModel.peek().ssh, fingerprint] })
    return { }
  }

  async deleteSSHKey (fingerprint: SSHFingerprint): Promise<EmptyResponse> {
    await mockDeleteSSHKey()
    const ssh = this.serverModel.peek().ssh
    this.serverModel.update({ ssh: ssh.filter(s => s !== fingerprint) })
    return { }
  }

  async addWifi (ssid: string, password: string, country: string, connect: boolean): Promise<EmptyResponse> {
    return mockAddWifi()
  }

  async connectWifi (ssid: string): Promise<EmptyResponse> {
    return mockConnectWifi()
  }

  async deleteWifi (ssid: string): Promise<EmptyResponse> {
    return mockDeleteWifi()
  }

  async restartServer (): Promise<EmptyResponse> {
    return mockRestartServer()
  }

  async shutdownServer (): Promise<EmptyResponse> {
    return mockShutdownServer()
  }
}

async function mockGetServer (): Promise<ReqRes.GetServerRes> {
  await pauseFor(1000)
  return mockApiServer()
}

async function mockGetVersionLatest (): Promise<ReqRes.GetVersionLatestRes> {
  await pauseFor(1000)
  return mockVersionLatest
}

async function mockGetServerMetrics (): Promise<ReqRes.GetServerMetricsRes> {
  await pauseFor(1000)
  return mockApiServerMetrics
}

async function mockGetNotifications (): Promise<ReqRes.GetNotificationsRes> {
  await pauseFor(1000)
  function cloneAndChange (arr: S9Notification[], letter: string) { return JSON.parse(JSON.stringify(arr)).map(a => { a.id = a.id + letter; return a }) }
  return mockApiNotifications.concat(cloneAndChange(mockApiNotifications, 'a')).concat(cloneAndChange(mockApiNotifications, 'b'))
}

async function mockDeleteNotification (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockGetExternalDisks (): Promise<ReqRes.GetExternalDisksRes> {
  await pauseFor(1000)
  return mockApiExternalDisks
}

async function mockPostUpdateAgent (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockGetAvailableApp (appId: string): Promise<ReqRes.GetAppAvailableRes> {
  await pauseFor(1000)
  return mockApiAppAvailableFull[appId]
}

async function mockGetAvailableApps (): Promise<ReqRes.GetAppsAvailableRes> {
  await pauseFor(1000)
  return Object.values(mockApiAppAvailableFull)
}

async function mockGetInstalledApp (appId: string): Promise<AppInstalledFull> {
  await pauseFor(1000)
  return { ...mockApiAppInstalledFull[appId], hasFetchedFull: true }
}

async function mockGetInstalledApps (): Promise<AppInstalledPreview[]> {
  await pauseFor(1000)
  return Object.values(mockApiAppInstalledFull).map(toInstalledPreview).filter(({ versionInstalled}) => !!versionInstalled)
}

async function mockGetAppLogs (): Promise<ReqRes.GetAppLogsRes> {
  await pauseFor(1000)
  return mockApiAppLogs
}

async function mockGetAppMetrics (): Promise<ReqRes.GetAppMetricsRes> {
  await pauseFor(1000)
  return mockApiAppMetricsV1
}

async function mockGetAvailableAppVersionInfo (): Promise<ReqRes.GetAppAvailableVersionInfoRes> {
  await pauseFor(1000)
  return mockApiAppAvailableVersionInfo
}

async function mockGetAppConfig (): Promise<ReqRes.GetAppConfigRes> {
  await pauseFor(1000)
  return mockApiAppConfig
}

async function mockInstallApp (appId: string): Promise<AppInstalledFull & { breakages: DependentBreakage[] }> {
  await pauseFor(1000)
  return { ...mockApiAppInstalledFull[appId], hasFetchedFull: true, ...mockAppDependentBreakages }
}

async function mockUninstallApp (): Promise< { breakages: DependentBreakage[] } > {
  await pauseFor(1000)
  return mockAppDependentBreakages
}

async function mockStartApp (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockStopApp (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockCreateAppBackup (): Promise<ReqRes.PostAppBackupCreateRes> {
  await pauseFor(1000)
  return { }
}

async function mockStopAppBackup (): Promise<ReqRes.PostAppBackupStopRes> {
  await pauseFor(1000)
  return { }
}


async function mockPatchAppConfig (): Promise<{ breakages: DependentBreakage[] }> {
  await pauseFor(1000)
  return mockAppDependentBreakages
}

async function mockPatchServerConfig (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockWipeAppData (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockAddSSHKey (): Promise<SSHFingerprint> {
  await pauseFor(1000)
  return mockApiServer().ssh[0]
}

async function mockDeleteSSHKey (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockAddWifi (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockConnectWifi (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockDeleteWifi (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockRestartServer (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

async function mockShutdownServer (): Promise<Unit> {
  await pauseFor(1000)
  return { }
}

const mockApiNotifications: ReqRes.GetNotificationsRes = [
  {
    id: '123e4567-e89b-12d3-a456-426655440000',
    appId: 'bitcoind',
    createdAt: '2019-12-26T14:20:30.872Z',
    code: '101',
    title: 'Install Complete',
    message: 'Installation of bitcoind has completed successfully.',
  },
  {
    id: '123e4567-e89b-12d3-a456-426655440001',
    appId: 'bitcoind',
    createdAt: '2019-12-26T14:20:30.872Z',
    code: '201',
    title: 'SSH Key Added',
    message: 'A new SSH key was added. If you did not do this, shit is bad.',
  },
  {
    id: '123e4567-e89b-12d3-a456-426655440002',
    appId: 'bitcoind',
    createdAt: '2019-12-26T14:20:30.872Z',
    code: '002',
    title: 'SSH Key Removed',
    message: 'A SSH key was removed.',
  },
  {
    id: '123e4567-e89b-12d3-a456-426655440003',
    appId: 'bitcoind',
    createdAt: '2019-12-26T14:20:30.872Z',
    code: '310',
    title: 'App Crashed',
    message: 'Bitcoind has crashed',
  },
]

const mockApiServer: () => ReqRes.GetServerRes = () => ({
  serverId: 'start9-mockxyzab',
  name: 'Embassy:12345678',
  versionInstalled: '0.2.6',
  status: ServerStatus.RUNNING,
  alternativeRegistryUrl: 'beta-registry.start9labs.com',
  specs: {
    'Tor Address': 'nfsnjkcnaskjnlkasnfahj7dh23fdnieqwjdnhjewbfijendiueqwbd.onion',
    'CPU': 'Broadcom BCM2711, Quad core Cortex-A72 (ARM v8) 64-bit SoC @ 1.5GHz',
    'RAM': '4GB LPDDR4-2400 SDRAM',
    'WiFI': '2.4 GHz and 5.0 GHz IEEE 802.11ac wireless, Bluetooth 5.0, BLE',
    'Ethernet': 'Gigabit',
    'Disk': '512 GB Flash (280 GB available)',
    'Embassy OS Version': '0.1.0.1',
  },
  wifi: {
    ssids: ['Goosers', 'Atlantic City'],
    current: 'Goosers',
  },
  ssh: [
    {
      alg: 'ed25519',
      hash: '28:d2:7e:78:61:b4:bf:g2:de:24:15:96:4e:d4:15:53',
      hostname: 'aaron key',
    },
    {
      alg: 'ed25519',
      hash: '12:f8:7e:78:61:b4:bf:e2:de:24:15:96:4e:d4:72:53',
      hostname: 'matt macbook pro',
    },
  ],
})

const mockVersionLatest: ReqRes.GetVersionLatestRes = {
  versionLatest: '0.2.6',
  canUpdate: true,
}

const mockApiServerMetrics: ReqRes.GetServerMetricsRes = {
  'Group1': {
    'Metric1': {
      value: 22.2,
      unit: 'mi/b',
    },
    'Metric2': {
      value: 50,
      unit: '%',
    },
    'Metric3': {
      value: 10.1,
      unit: '%',
    },
  },
  'Group2': {
    'Hmmmm1': {
      value: 22.2,
      unit: 'mi/b',
    },
    'Hmmmm2': {
      value: 50,
      unit: '%',
    },
    'Hmmmm3': {
      value: 10.1,
      unit: '%',
    },
  },
}

const mockApiExternalDisks: DiskInfo[] = [
  {
    logicalname: '/dev/sda',
    size: '32GB',
    description: 'Samsung',
    partitions: [
      {
        logicalname: 'sdba2',
        size: null,
        isMounted: false,
        label: 'Matt Stuff',
      },
    ],
  },
  {
    logicalname: '/dev/sba',
    size: '64GB',
    description: 'small USB stick',
    partitions: [
      {
        logicalname: 'sdba2',
        size: '16GB',
        isMounted: true,
        label: null,
      },
    ],
  },
  {
    logicalname: '/dev/sbd',
    size: '128GBGB',
    description: 'large USB stick',
    partitions: [
      {
        logicalname: 'sdba1',
        size: '32GB',
        isMounted: true,
        label: 'Partition 1',
      },
      {
        logicalname: 'sdba2',
        size: null,
        isMounted: true,
        label: 'Partition 2',
      },
    ],
  },
]

const mockApiAppLogs: string[] = [
  '****** START *****',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:20:30.872Z - Hash: 2b2e5abb3cba2164aea0',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1244ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:21:01.685Z - Hash: bb3f5d0e11f2cd2dd57b',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1185ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:23:13.812Z - Hash: 9342e11e6b8e16ad2f70',
  '[ng] 114 unchanged chunks',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:20:30.872Z - Hash: 2b2e5abb3cba2164aea0',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1244ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:21:01.685Z - Hash: bb3f5d0e11f2cd2dd57b',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1185ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:23:13.812Z - Hash: 9342e11e6b8e16ad2f70',
  '[ng] 114 unchanged chunks',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:20:30.872Z - Hash: 2b2e5abb3cba2164aea0',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1244ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:21:01.685Z - Hash: bb3f5d0e11f2cd2dd57b',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1185ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:23:13.812Z - Hash: 9342e11e6b8e16ad2f70',
  '[ng] 114 unchanged chunks',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:20:30.872Z - Hash: 2b2e5abb3cba2164aea0',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1244ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:21:01.685Z - Hash: bb3f5d0e11f2cd2dd57b',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1185ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:23:13.812Z - Hash: 9342e11e6b8e16ad2f70',
  '[ng] 114 unchanged chunks',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:20:30.872Z - Hash: 2b2e5abb3cba2164aea0',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1244ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:21:01.685Z - Hash: bb3f5d0e11f2cd2dd57b',
  '[ng] 114 unchanged chunks',
  '[ng] chunk {app-logs-app-logs-module} app-logs-app-logs-module.js, app-logs-app-logs-module.js.map (app-logs-app-logs-module) 7.86 kB  [rendered]',
  '[ng] Time: 1185ms',
  '[ng] ℹ ｢wdm｣: Compiled successfully.',
  '[ng] ℹ ｢wdm｣: Compiling...',
  '[ng] Date: 2019-12-26T14:23:13.812Z - Hash: 9342e11e6b8e16ad2f70',
  '[ng] 114 unchanged chunks',
  '****** FINISH *****',
]

const mockApiAppMetricsV1: AppMetricsVersioned<2> = {
  version: 2,
  data: {
    'Test': {
      type: 'string',
      description: 'This is some information about the thing.',
      copyable: true,
      qr: true,
      masked: false,
      value: 'lndconnect://udlyfq2mxa4355pt7cqlrdipnvk2tsl4jtsdw7zaeekenufwcev2wlad.onion:10009?cert=MIICJTCCAcugAwIBAgIRAOyq85fqAiA3U3xOnwhH678wCgYIKoZIzj0EAwIwODEfMB0GAkUEChMWbG5kIGF1dG9nZW5lcmF0ZWQgY2VydDEVMBMGA1UEAxMMNTc0OTkwMzIyYzZlMB4XDTIwMTAyNjA3MzEyN1oXDTIxMTIyMTA3MzEyN1owODEfMB0GA1UEChMWbG5kIGF1dG9nZW5lcmF0ZWQgY2VydDEVMBMGA1UEAxMMNTc0OTkwMzIyYzZlMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEKqfhAMMZdY-eFnU5P4bGrQTSx0lo7m8u4V0yYkzUM6jlql_u31_mU2ovLTj56wnZApkEjoPl6fL2yasZA2wiy6OBtTCBsjAOBgNVHQ8BAf8EBAMCAqQwEwYDVR0lBAwwCgYIKwYBBQUHAwEwDwYDVR0TAQH_BAUwAwEB_zAdBgNVHQ4EFgQUYQ9uIO6spltnVCx4rLFL5BvBF9IwWwYDVR0RBFQwUoIMNTc0OTkwMzIyYzZlgglsb2NhbGhvc3SCBHVuaXiCCnVuaXhwYWNrZXSCB2J1ZmNvbm6HBH8AAAGHEAAAAAAAAAAAAAAAAAAAAAGHBKwSAAswCgYIKoZIzj0EAwIDSAAwRQIgVZH2Z2KlyAVY2Q2aIQl0nsvN-OEN49wreFwiBqlxNj4CIQD5_JbpuBFJuf81I5J0FQPtXY-4RppWOPZBb-y6-rkIUQ&macaroon=AgEDbG5kAusBAwoQuA8OUMeQ8Fr2h-f65OdXdRIBMBoWCgdhZGRyZXNzEgRyZWFkEgV3cml0ZRoTCgRpbmZvEgRyZWFkEgV3cml0ZRoXCghpbnZvaWNlcxIEcmVhZBIFd3JpdGUaFAoIbWFjYXJvb24SCGdlbmVyYXRlGhYKB21lc3NhZ2USBHJlYWQSBXdyaXRlGhcKCG9mZmNoYWluEgRyZWFkEgV3cml0ZRoWCgdvbmNoYWluEgRyZWFkEgV3cml0ZRoUCgVwZWVycxIEcmVhZBIFd3JpdGUaGAoGc2lnbmVyEghnZW5lcmF0ZRIEcmVhZAAABiCYsRUoUWuAHAiCSLbBR7b_qULDSl64R8LIU2aqNIyQfA',
    },
    'Nested': {
      type: 'object',
      description: 'This is a nested thing metric',
      value: {
        'Last Name': {
          type: 'string',
          description: 'The last name of the user',
          copyable: true,
          qr: true,
          masked: false,
          value: 'Hill',
        },
        'Age': {
          type: 'string',
          description: 'The age of the user',
          copyable: false,
          qr: false,
          masked: false,
          value: '35',
        },
        'Password': {
          type: 'string',
          description: 'A secret password',
          copyable: true,
          qr: false,
          masked: true,
          value: 'password123',
        },
      },
    },
    'Another Property': {
      type: 'string',
      description: 'Some more information about the service.',
      copyable: false,
      qr: true,
      masked: false,
      value: 'https://guessagain.com',
    },
  },
}

const mockApiAppConfig: ReqRes.GetAppConfigRes = {
  // config spec
  spec: {
    'testnet': {
      'name': 'Testnet',
      'type': 'boolean',
      'description': 'determines whether your node is running ontestnet or mainnet',
      'changeWarning': 'Chain will have to resync!',
      'default': false,
    },
    'objectList': {
      'name': 'Object List',
      'type': 'list',
      'subtype': 'object',
      'description': 'This is a list of objects, like users or something',
      'range': '[0,4]',
      'default': [
        {
          'firstName': 'Admin',
          'lastName': 'User',
          'age': 40,
        },
        {
          'firstName': 'Admin2',
          'lastName': 'User2',
          'age': 40,
        },
      ],
      // the outer spec here, at the list level, says that what's inside (the inner spec) pertains to its inner elements.
      // it just so happens that ValueSpecObject's have the field { spec: ConfigSpec }
      // see 'unionList' below for a different example.
      'spec': {
        'uniqueBy': 'lastName',
        'displayAs': `I'm {{lastName}}, {{firstName}} {{lastName}}`,
        'spec': {
          'firstName': {
            'name': 'First Name',
            'type': 'string',
            'description': 'User first name',
            'nullable': true,
            'default': null,
            'masked': false,
            'copyable': false,
          },
          'lastName': {
            'name': 'Last Name',
            'type': 'string',
            'description': 'User first name',
            'nullable': true,
            'default': {
              'charset': 'a-g,2-9',
              'len': 12,
            },
            'pattern': '^[a-zA-Z]+$',
            'patternDescription': 'must contain only letters.',
            'masked': false,
            'copyable': true,
          },
          'age': {
            'name': 'Age',
            'type': 'number',
            'description': 'The age of the user',
            'nullable': true,
            'default': null,
            'integral': false,
            'changeWarning': 'User must be at least 18.',
            'range': '[18,*)',
          },
        },
      },
    },
    'unionList': {
      'name': 'Union List',
      'type': 'list',
      'subtype': 'union',
      'description': 'This is a sample list of unions',
      'changeWarning': 'If you change this, things may work.',
      // a list of union selections. e.g. 'summer', 'winter',...
      'default': [
        'summer',
      ],
      'range': '[0, 2]',
      'spec': {
          'tag': {
            'id': 'preference',
            'name': 'Preferences',
            'variantNames': {
              'summer': 'Summer',
              'winter': 'Winter',
              'other': 'Other',
            },
          },
          // this default is used to make a union selection when a new list element is first created
          'default': 'summer',
          'variants': {
              'summer': {
                'favorite-tree': {
                  'name': 'Favorite Tree',
                  'type': 'string',
                  'nullable': false,
                  'description': 'What is your favorite tree?',
                  'default': 'Maple',
                  'masked': false,
                  'copyable': false,
                },
                'favorite-flower': {
                  'name': 'Favorite Flower',
                  'type': 'enum',
                  'description': 'Select your favorite flower',
                  'valueNames': {
                    'none': 'Hate Flowers',
                    'red': 'Red',
                    'blue': 'Blue',
                    'purple': 'Purple',
                  },
                  'values': [
                    'none',
                    'red',
                    'blue',
                    'purple',
                  ],
                  'default': 'none',
                },
              },
              'winter': {
                'like-snow': {
                  'name': 'Like Snow?',
                  'type': 'boolean',
                  'description': 'Do you like snow or not?',
                  'default': true,
                },
              },
        },
        'uniqueBy': 'preference',
      },
    },
    'randomEnum': {
      'name': 'Random Enum',
      'type': 'enum',
      'valueNames': {
        'null': 'Null',
        'option1': 'One 1',
        'option2': 'Two 2',
        'option3': 'Three 3',
      },
      'default': 'null',
      'description': 'This is not even real.',
      'changeWarning': 'Be careful chnaging this!',
      'values': [
        'null',
        'option1',
        'option2',
        'option3',
      ],
    },
    'favoriteNumber': {
      'name': 'Favorite Number',
      'type': 'number',
      'integral': false,
      'description': 'Your favorite number of all time',
      'changeWarning': 'Once you set this number, it can never be changed without severe consequences.',
      'nullable': false,
      'default': 7,
      'range': '(-100,100]',
      'units': 'BTC',
    },
    'secondaryNumbers': {
      'name': 'Unlucky Numbers',
      'type': 'list',
      'subtype': 'number',
      'description': 'Numbers that you like but are not your top favorite.',
      'spec': {
        'integral': false,
        'range': '[-100,200)',
      },
      'range': '[0,10]',
      'default': [
        2,
        3,
      ],
    },
    'rpcsettings': {
      'name': 'RPC Settings',
      'type': 'object',
      'uniqueBy': null,
      'description': 'rpc username and password',
      'changeWarning': 'Adding RPC users gives them special permissions on your node.',
      'nullable': false,
      'nullByDefault': false,
      'spec': {
        'laws': {
          'name': 'Laws',
          'type': 'object',
          'uniqueBy': 'law1',
          'description': 'the law of the realm',
          'nullable': true,
          'nullByDefault': true,
          'spec': {
            'law1': {
              'name': 'First Law',
              'type': 'string',
              'description': 'the first law',
              'nullable': true,
              'masked': false,
              'copyable': true,
            },
            'law2': {
              'name': 'Second Law',
              'type': 'string',
              'description': 'the second law',
              'nullable': true,
              'masked': false,
              'copyable': true,
            },
          },
        },
        'rulemakers': {
          'name': 'Rule Makers',
          'type': 'list',
          'subtype': 'object',
          'description': 'the people who make the rules',
          'range': '[0,2]',
          'default': [],
          'spec': {
            'uniqueBy': null,
            'spec': {
              'rulemakername': {
                'name': 'Rulemaker Name',
                'type': 'string',
                'description': 'the name of the rule maker',
                'nullable': false,
                'default': {
                  'charset': 'a-g,2-9',
                  'len': 12,
                },
                'masked': false,
                'copyable': false,
              },
              'rulemakerip': {
                'name': 'Rulemaker IP',
                'type': 'string',
                'description': 'the ip of the rule maker',
                'nullable': false,
                'default': '192.168.1.0',
                'pattern': '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                'patternDescription': 'may only contain numbers and periods',
                'masked': false,
                'copyable': true,
              },
            },
          },
        },
        'rpcuser': {
          'name': 'RPC Username',
          'type': 'string',
          'description': 'rpc username',
          'nullable': false,
          'default': 'defaultrpcusername',
          'pattern': '^[a-zA-Z]+$',
          'patternDescription': 'must contain only letters.',
          'masked': false,
          'copyable': true,
        },
        'rpcpass': {
          'name': 'RPC User Password',
          'type': 'string',
          'description': 'rpc password',
          'nullable': false,
          'default': {
            'charset': 'a-z,A-Z,2-9',
            'len': 20,
          },
          'masked': true,
          'copyable': true,
        },
      },
    },
    'advanced': {
      'name': 'Advanced',
      'type': 'object',
      'uniqueBy': null,
      'description': 'Advanced settings',
      'nullable': false,
      'nullByDefault': false,
      'spec': {
        'notifications': {
          'name': 'Notification Preferences',
          'type': 'list',
          'subtype': 'enum',
          'description': 'how you want to be notified',
          'range': '[1,3]',
          'default': [
            'email',
          ],
          'spec': {
            'valueNames': {
              'email': 'EEEEmail',
              'text': 'Texxxt',
              'call': 'Ccccall',
              'push': 'PuuuusH',
              'webhook': 'WebHooookkeee',
            },
            'values': [
              'email',
              'text',
              'call',
              'push',
              'webhook',
            ],
          },
        },
      },
    },
    'bitcoinNode': {
      'name': 'Bitcoin Node Settings',
      'type': 'union',
      'uniqueBy': null,
      'description': 'The node settings',
      'default': 'internal',
      'changeWarning': 'Careful changing this',
      'tag': {
          'id': 'type',
          'name': 'Type',
          'variantNames': {
            'internal': 'Internal',
            'external': 'External',
          },
      },
      'variants': {
        'internal': {
          'lan-address': {
            'name': 'LAN Address',
            'type': 'pointer',
            'subtype': 'app',
            'target': 'lan-address',
            'app-id': 'bitcoind',
            'description': 'the lan address',
          },
        },
        'external': {
          'public-domain': {
            'name': 'Public Domain',
            'type': 'string',
            'description': 'the public address of the node',
            'nullable': false,
            'default': 'bitcoinnode.com',
            'pattern': '.*',
            'patternDescription': 'anything',
            'masked': false,
            'copyable': true,
          },
        },
      },
    },
    'port': {
      'name': 'Port',
      'type': 'number',
      'integral': true,
      'description': 'the default port for your Bitcoin node. default: 8333, testnet: 18333, regtest: 18444',
      'nullable': false,
      'default': 8333,
      'range': '[0, 9999]',
    },
    'favoriteSlogan': {
      'name': 'Favorite Slogan',
      'type': 'string',
      'description': 'You most favorite slogan in the whole world, used for paying you.',
      'nullable': true,
      'masked': true,
      'copyable': true,
    },
    'rpcallowip': {
      'name': 'RPC Allowed IPs',
      'type': 'list',
      'subtype': 'string',
      'description': 'external ip addresses that are authorized to access your Bitcoin node',
      'changeWarning': 'Any IP you allow here will have RPC access to your Bitcoin node.',
      'range': '[1,10]',
      'default': [
        '192.168.1.1',
      ],
      'spec': {
        'pattern': '((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))',
        'patternDescription': 'must be a valid ipv4, ipv6, or domain name',
      },
    },
    'rpcauth': {
      'name': 'RPC Auth',
      'type': 'list',
      'subtype': 'string',
      'description': 'api keys that are authorized to access your Bitcoin node.',
      'range': '[0,*)',
      'default': [],
      'spec': { },
    },
  },
  // actual config
  config: {
    testnet: undefined,
    objectList: undefined,
    unionList: undefined,
    randomEnum: 'option1',
    favoriteNumber: 8,
    secondaryNumbers: undefined,
    rpcsettings: {
      laws: null,
      rpcpass: null,
      rpcuser: '123',
      rulemakers: [],
    },
    advanced: {
      notifications: ['call'],
    },
    bitcoinNode: undefined,
    port: 5959,
    maxconnections: null,
    rpcallowip: undefined,
    rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
  },
  rules: [],
}

export const mockCupsDependentConfig = {
  randomEnum: 'option1',
  testnet: false,
  favoriteNumber: 8,
  secondaryNumbers: [13, 58, 20],
  objectList: [],
  unionList: [],
  rpcsettings: {
    laws: null,
    rpcpass: null,
    rpcuser: '123',
    rulemakers: [],
  },
  advanced: {
    notifications: [],
  },
  bitcoinNode: { type: 'internal' },
  port: 5959,
  maxconnections: null,
  rpcallowip: [],
  rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
}