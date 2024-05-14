import {
  InstalledState,
  PackageDataEntry,
  ServerStatusInfo,
} from 'src/app/services/patch-db/data-model'
import {
  Metrics,
  NotificationLevel,
  RR,
  ServerNotifications,
} from './api.types'
import { DependencyMetadata, MarketplacePkg } from '@start9labs/marketplace'
import { Log } from '@start9labs/shared'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { CT, T, CB } from '@start9labs/start-sdk'

const BTC_ICON = '/assets/img/service-icons/bitcoind.svg'
const LND_ICON = '/assets/img/service-icons/lnd.png'
const PROXY_ICON = '/assets/img/service-icons/btc-rpc-proxy.png'

export module Mock {
  export const ServerUpdated: ServerStatusInfo = {
    currentBackup: null,
    updateProgress: null,
    updated: true,
    restarting: false,
    shuttingDown: false,
  }
  export const MarketplaceEos: RR.GetMarketplaceEosRes = {
    version: '0.3.5.2',
    headline: 'Our biggest release ever.',
    releaseNotes: {
      '0.3.5.2': 'Some **Markdown** release _notes_ for 0.3.5.2',
      '0.3.5.1': 'Some **Markdown** release _notes_ for 0.3.5.1',
      '0.3.4.4': 'Some **Markdown** release _notes_ for 0.3.4.4',
      '0.3.4.3': 'Some **Markdown** release _notes_ for 0.3.4.3',
      '0.3.4.2': 'Some **Markdown** release _notes_ for 0.3.4.2',
      '0.3.4.1': 'Some **Markdown** release _notes_ for 0.3.4.1',
      '0.3.4': 'Some **Markdown** release _notes_ for 0.3.4',
      '0.3.3': 'Some **Markdown** release _notes_ for 0.3.3',
      '0.3.2.1': 'Some **Markdown** release _notes_ for 0.3.2.1',
      '0.3.2': 'Some **Markdown** release _notes_ for 0.3.2',
      '0.3.1': 'Some **Markdown** release _notes_ for 0.3.1',
      '0.3.0': 'Some **Markdown** release _notes_ from a prior version',
    },
  }

  export const ReleaseNotes: RR.GetReleaseNotesRes = {
    '0.19.2':
      'Contrary to popular belief, Lorem Ipsum is not simply random text.',
    '0.19.1': 'release notes for Bitcoin 0.19.1',
    '0.19.0': 'release notes for Bitcoin 0.19.0',
  }

  export const MockManifestBitcoind: T.Manifest = {
    id: 'bitcoind',
    title: 'Bitcoin Core',
    version: '0.21.0',
    gitHash: 'abcdefgh',
    description: {
      short: 'A Bitcoin full node by Bitcoin Core.',
      long: 'Bitcoin is a decentralized consensus protocol and settlement network.',
    },
    replaces: ['banks', 'governments'],
    releaseNotes: 'Taproot, Schnorr, and more.',
    license: 'MIT',
    wrapperRepo: 'https://github.com/start9labs/bitcoind-wrapper',
    upstreamRepo: 'https://github.com/bitcoin/bitcoin',
    supportSite: 'https://bitcoin.org',
    marketingSite: 'https://bitcoin.org',
    donationUrl: 'https://start9.com',
    alerts: {
      install: 'Bitcoin can take over a week to sync.',
      uninstall:
        'Chain state will be lost, as will any funds stored on your Bitcoin Core waller that have not been backed up.',
      restore: null,
      start: 'Starting Bitcoin is good for your health.',
      stop: null,
    },
    osVersion: '0.2.12',
    dependencies: {},
    hasConfig: true,
    images: ['main'],
    assets: [],
    volumes: ['main'],
    hardwareRequirements: {
      device: {},
      arch: null,
      ram: null,
    },
  }

  export const MockManifestLnd: T.Manifest = {
    id: 'lnd',
    title: 'Lightning Network Daemon',
    version: '0.11.1',
    gitHash: 'abcdefgh',
    description: {
      short: 'A bolt spec compliant client.',
      long: 'More info about LND. More info about LND. More info about LND.',
    },
    replaces: ['banks', 'governments'],
    releaseNotes: 'Dual funded channels!',
    license: 'MIT',
    wrapperRepo: 'https://github.com/start9labs/lnd-wrapper',
    upstreamRepo: 'https://github.com/lightningnetwork/lnd',
    supportSite: 'https://lightning.engineering/',
    marketingSite: 'https://lightning.engineering/',
    donationUrl: null,
    alerts: {
      install: null,
      uninstall: null,
      restore:
        'If this is a duplicate instance of the same LND node, you may loose your funds.',
      start: 'Starting LND is good for your health.',
      stop: null,
    },
    osVersion: '0.2.12',
    dependencies: {
      bitcoind: {
        description: 'LND needs bitcoin to live.',
        optional: true,
      },
      'btc-rpc-proxy': {
        description:
          'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
        optional: true,
      },
    },
    hasConfig: true,
    images: ['main'],
    assets: [],
    volumes: ['main'],
    hardwareRequirements: {
      device: {},
      arch: null,
      ram: null,
    },
  }

  export const MockManifestBitcoinProxy: T.Manifest = {
    id: 'btc-rpc-proxy',
    title: 'Bitcoin Proxy',
    version: '0.2.2',
    gitHash: 'lmnopqrx',
    description: {
      short: 'A super charger for your Bitcoin node.',
      long: 'More info about Bitcoin Proxy. More info about Bitcoin Proxy. More info about Bitcoin Proxy.',
    },
    releaseNotes: 'Even better support for Bitcoin and wallets!',
    license: 'MIT',
    wrapperRepo: 'https://github.com/start9labs/btc-rpc-proxy-wrapper',
    upstreamRepo: 'https://github.com/Kixunil/btc-rpc-proxy',
    supportSite: '',
    marketingSite: '',
    donationUrl: 'https://start9.com',
    alerts: {
      install: 'Testing install alert',
      uninstall: null,
      restore: null,
      start: null,
      stop: null,
    },
    osVersion: '0.2.12',
    dependencies: {
      bitcoind: {
        description: 'Bitcoin Proxy requires a Bitcoin node.',
        optional: false,
      },
    },
    replaces: [],
    hasConfig: false,
    images: ['main'],
    assets: [],
    volumes: ['main'],
    hardwareRequirements: {
      device: {},
      arch: null,
      ram: null,
    },
  }

  export const BitcoinDep: DependencyMetadata = {
    title: 'Bitcoin Core',
    icon: BTC_ICON,
    optional: false,
    hidden: true,
  }

  export const ProxyDep: DependencyMetadata = {
    title: 'Bitcoin Proxy',
    icon: PROXY_ICON,
    optional: true,
    hidden: false,
  }

  export const MarketplacePkgs: {
    [id: string]: {
      [version: string]: MarketplacePkg
    }
  } = {
    bitcoind: {
      '0.19.0': {
        icon: BTC_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.19.0',
        },
        categories: ['bitcoin', 'cryptocurrency', 'featured'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        dependencyMetadata: {},
        publishedAt: new Date().toISOString(),
      },
      '0.20.0': {
        icon: BTC_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        dependencyMetadata: {},
        publishedAt: new Date().toISOString(),
      },
      '0.21.0': {
        icon: BTC_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.21.0',
          releaseNotes:
            'For a complete list of changes, please visit <a href="https://bitcoincore.org/en/releases/0.21.0/">https://bitcoincore.org/en/releases/0.21.0/</a><br /><ul><li>Taproot!</li><li>New RPCs</li><li>Experimental Descriptor Wallets</li></ul>',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        dependencyMetadata: {},
        publishedAt: new Date().toISOString(),
      },
      latest: {
        icon: BTC_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        screenshots: ['one.png', 'two.png', 'three.png'],
        manifest: {
          ...Mock.MockManifestBitcoind,
          releaseNotes:
            'For a complete list of changes, please visit <a href="https://bitcoincore.org/en/releases/0.21.0/" target="_blank">https://bitcoincore.org/en/releases/0.21.0/</a><br />Or in [markdown](https://bitcoincore.org/en/releases/0.21.0/)<ul><li>Taproot!</li><li>New RPCs</li><li>Experimental Descriptor Wallets</li></ul>',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        dependencyMetadata: {},
        publishedAt: new Date().toISOString(),
      },
    },
    lnd: {
      '0.11.0': {
        icon: LND_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.0',
          releaseNotes: 'release notes for LND 0.11.0',
        },
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        dependencyMetadata: {
          bitcoind: BitcoinDep,
          'btc-rpc-proxy': ProxyDep,
        },
        publishedAt: new Date().toISOString(),
      },
      '0.11.1': {
        icon: LND_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.1',
          releaseNotes: 'release notes for LND 0.11.1',
        },
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        dependencyMetadata: {
          bitcoind: BitcoinDep,
          'btc-rpc-proxy': ProxyDep,
        },
        publishedAt: new Date().toISOString(),
      },
      latest: {
        icon: LND_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestLnd,
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        dependencyMetadata: {
          bitcoind: BitcoinDep,
          'btc-rpc-proxy': ProxyDep,
        },
        publishedAt: new Date(new Date().valueOf() + 10).toISOString(),
      },
    },
    'btc-rpc-proxy': {
      latest: {
        icon: PROXY_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestBitcoinProxy,
        categories: ['bitcoin'],
        versions: ['0.2.2'],
        dependencyMetadata: {
          bitcoind: BitcoinDep,
        },
        publishedAt: new Date().toISOString(),
      },
    },
  }

  export const marketplacePkgsList = (
    version?: string,
  ): RR.GetMarketplacePackagesRes => {
    return Object.values(Mock.MarketplacePkgs).map(service =>
      version ? service[version] : service['latest'],
    )
  }

  export const Notifications: ServerNotifications = [
    {
      id: 1,
      packageId: null,
      createdAt: '2019-12-26T14:20:30.872Z',
      code: 1,
      level: NotificationLevel.Success,
      title: 'Backup Complete',
      message: 'StartOS and services have been successfully backed up.',
      data: {
        server: {
          attempted: false,
          error: null,
        },
        packages: {
          bitcoind: {
            error: 'An error ocurred while backing up',
          },
        },
      },
      read: false,
    },
    {
      id: 2,
      packageId: null,
      createdAt: '2019-12-26T14:20:30.872Z',
      code: 2,
      level: NotificationLevel.Warning,
      title: 'SSH Key Added',
      message: 'A new SSH key was added. If you did not do this, shit is bad.',
      data: null,
      read: false,
    },
    {
      id: 3,
      packageId: null,
      createdAt: '2019-12-26T14:20:30.872Z',
      code: 3,
      level: NotificationLevel.Info,
      title: 'SSH Key Removed',
      message: 'A SSH key was removed.',
      data: null,
      read: false,
    },
    {
      id: 4,
      packageId: 'bitcoind',
      createdAt: '2019-12-26T14:20:30.872Z',
      code: 4,
      level: NotificationLevel.Error,
      title: 'Service Crashed',
      message: new Array(3)
        .fill(
          `2021-11-27T18:36:30.451064Z 2021-11-27T18:36:30Z tor: Thread interrupt
        2021-11-27T18:36:30.452833Z 2021-11-27T18:36:30Z Shutdown: In progress...
        2021-11-27T18:36:30.453128Z 2021-11-27T18:36:30Z addcon thread exit
        2021-11-27T18:36:30.453369Z 2021-11-27T18:36:30Z torcontrol thread exit`,
        )
        .join(''),
      data: null,
      read: false,
    },
  ]

  export function getMetrics(): Metrics {
    return {
      general: {
        temperature: {
          value: '66.8',
          unit: '°C',
        },
      },
      memory: {
        percentageUsed: {
          value: '30.7',
          unit: '%',
        },
        total: {
          value: '31971.10',
          unit: 'MiB',
        },
        available: {
          value: '22150.66',
          unit: 'MiB',
        },
        used: {
          value: '8784.97',
          unit: 'MiB',
        },
        zramTotal: {
          value: '7992.00',
          unit: 'MiB',
        },
        zramAvailable: {
          value: '7882.50',
          unit: 'MiB',
        },
        zramUsed: {
          value: '109.50',
          unit: 'MiB',
        },
      },
      cpu: {
        percentageUsed: {
          value: '8.4',
          unit: '%',
        },
        userSpace: {
          value: '7.0',
          unit: '%',
        },
        kernelSpace: {
          value: '1.4',
          unit: '%',
        },
        wait: {
          value: '0.5',
          unit: '%',
        },
        idle: {
          value: '91.1',
          unit: '%',
        },
      },
      disk: {
        capacity: {
          value: '1851.60',
          unit: 'GB',
        },
        used: {
          value: '859.02',
          unit: 'GB',
        },
        available: {
          value: '992.59',
          unit: 'GB',
        },
        percentageUsed: {
          value: '46.4',
          unit: '%',
        },
      },
    }
  }

  export const ServerLogs: Log[] = [
    {
      timestamp: '2022-07-28T03:52:54.808769Z',
      message: '****** START *****',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      message:
        '\u001b[34mPOST \u001b[0;32;49m200\u001b[0m photoview.startos/api/graphql \u001b[0;36;49m1.169406ms\u001b',
    },
    {
      timestamp: '2019-12-26T14:22:30.872Z',
      message: '****** FINISH *****',
    },
  ]

  export const PackageLogs: Log[] = [
    {
      timestamp: '2022-07-28T03:52:54.808769Z',
      message: '****** START *****',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      message: 'PackageLogs PackageLogs PackageLogs PackageLogs PackageLogs',
    },
    {
      timestamp: '2019-12-26T14:22:30.872Z',
      message: '****** FINISH *****',
    },
  ]

  export const Sessions: RR.GetSessionsRes = {
    current: 'b7b1a9cef4284f00af9e9dda6e676177',
    sessions: {
      c54ddd8107d6d7b9d8aed7: {
        lastActive: '2021-07-14T20:49:17.774Z',
        userAgent: 'AppleWebKit/{WebKit Rev} (KHTML, like Gecko)',
        metadata: {
          platforms: ['iphone', 'mobileweb', 'mobile', 'ios'],
        },
      },
      klndsfjhbwsajkdnaksj: {
        lastActive: '2019-07-14T20:49:17.774Z',
        userAgent: 'AppleWebKit/{WebKit Rev} (KHTML, like Gecko)',
        metadata: {
          platforms: ['cli'],
        },
      },
      b7b1a9cef4284f00af9e9dda6e676177: {
        lastActive: '2021-06-14T20:49:17.774Z',
        userAgent:
          'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0',
        metadata: {
          platforms: ['desktop'],
        },
      },
    },
  }

  export const ActionResponse: RR.ExecutePackageActionRes = {
    message:
      'Password changed successfully. If you lose your new password, you will be lost forever.',
    value: 'NewPassword1234!',
    copyable: true,
    qr: true,
  }

  export const SshKeys: RR.GetSSHKeysRes = [
    {
      createdAt: new Date().toISOString(),
      alg: 'ed25519',
      hostname: 'Matt Key',
      fingerprint: '28:d2:7e:78:61:b4:bf:g2:de:24:15:96:4e:d4:15:53',
    },
    {
      createdAt: new Date().toISOString(),
      alg: 'ed25519',
      hostname: 'Aiden Key',
      fingerprint: '12:f8:7e:78:61:b4:bf:e2:de:24:15:96:4e:d4:72:53',
    },
  ]

  export const SshKey: RR.AddSSHKeyRes = {
    createdAt: new Date().toISOString(),
    alg: 'ed25519',
    hostname: 'Lucy Key',
    fingerprint: '44:44:7e:78:61:b4:bf:g2:de:24:15:96:4e:d4:15:53',
  }

  export const Wifi: RR.GetWifiRes = {
    ethernet: true,
    ssids: {
      Goosers: 50,
      Goosers5G: 0,
    },
    connected: 'Goosers',
    country: 'US',
    availableWifi: [
      {
        ssid: 'Goosers a billion',
        strength: 40,
        security: [],
      },
      {
        ssid: 'Bill nye the wifi guy',
        strength: 99,
        security: ['1', '2', '3'],
      },
      {
        ssid: '',
        strength: 40,
        security: [],
      },
    ],
  }

  export const BackupTargets: RR.GetBackupTargetsRes = {
    unknownDisks: [
      {
        logicalname: 'sbc4',
        label: 'My Backup Drive',
        capacity: 2000000000000,
        used: 100000000000,
        model: 'T7',
        vendor: 'Samsung',
        startOs: null,
      },
    ],
    saved: [
      {
        id: 'hsbdjhasbasda',
        type: 'cifs',
        name: 'Embassy Backups',
        hostname: 'smb://192.169.10.0',
        path: '/Desktop/embassy-backups',
        username: 'TestUser',
        mountable: false,
        startOs: {
          version: '0.3.0',
          full: true,
          passwordHash:
            // password is asdfasdf
            '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
          wrappedKey: '',
        },
      },
      {
        id: 'ftcvewdnkemfksdm',
        type: 'cloud',
        name: 'Dropbox 1',
        provider: 'dropbox',
        path: '/Home/backups',
        mountable: true,
        startOs: null,
      },
      {
        id: 'csgashbdjkasnd',
        type: 'cifs',
        name: 'Network Folder 2',
        hostname: 'smb://192.169.10.0',
        path: '/Desktop/embassy-backups-2',
        username: 'TestUser',
        mountable: true,
        startOs: null,
      },
      {
        id: 'powjefhjbnwhdva',
        type: 'disk',
        name: 'Physical Drive 1',
        logicalname: 'sdba1',
        label: 'Another Drive',
        capacity: 2000000000000,
        used: 100000000000,
        model: null,
        vendor: 'SSK',
        mountable: true,
        path: '/HomeFolder/Documents',
        startOs: {
          version: '0.3.0',
          full: true,
          // password is asdfasdf
          passwordHash:
            '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
          wrappedKey: '',
        },
      },
    ],
  }

  export const BackupJobs: RR.GetBackupJobsRes = [
    {
      id: 'lalalalalala-babababababa',
      name: 'My Backup Job',
      target: BackupTargets.saved[0],
      cron: '0 3 * * *',
      packageIds: ['bitcoind', 'lnd'],
    },
    {
      id: 'hahahahaha-mwmwmwmwmwmw',
      name: 'Another Backup Job',
      target: BackupTargets.saved[1],
      cron: '0 * * * *',
      packageIds: ['lnd'],
    },
  ]

  export const BackupRuns: RR.GetBackupRunsRes = [
    {
      id: 'kladhbfweubdsk',
      startedAt: new Date().toISOString(),
      completedAt: new Date(new Date().valueOf() + 10000).toISOString(),
      packageIds: ['bitcoind', 'lnd'],
      job: BackupJobs[0],
      report: {
        server: {
          attempted: true,
          error: null,
        },
        packages: {
          bitcoind: { error: null },
          lnd: { error: null },
        },
      },
    },
    {
      id: 'kladhbfwhrfeubdsk',
      startedAt: new Date().toISOString(),
      completedAt: new Date(new Date().valueOf() + 10000).toISOString(),
      packageIds: ['bitcoind', 'lnd'],
      job: BackupJobs[0],
      report: {
        server: {
          attempted: true,
          error: null,
        },
        packages: {
          bitcoind: { error: null },
          lnd: { error: null },
        },
      },
    },
  ]

  export const BackupInfo: RR.GetBackupInfoRes = {
    version: '0.3.0',
    timestamp: new Date().toISOString(),
    packageBackups: {
      bitcoind: {
        title: 'Bitcoin Core',
        version: '0.21.0',
        osVersion: '0.3.0',
        timestamp: new Date().toISOString(),
      },
      'btc-rpc-proxy': {
        title: 'Bitcoin Proxy',
        version: '0.2.2',
        osVersion: '0.3.0',
        timestamp: new Date().toISOString(),
      },
    },
  }

  export const getInputSpec = async (): Promise<
    RR.GetPackageConfigRes['spec']
  > =>
    configBuilderToSpec(
      CB.Config.of({
        bitcoin: CB.Value.object(
          {
            name: 'Bitcoin Settings',
            description:
              'RPC and P2P interface configuration options for Bitcoin Core',
          },
          CB.Config.of({
            'bitcoind-p2p': CB.Value.union(
              {
                name: 'P2P Settings',
                description:
                  '<p>The Bitcoin Core node to connect to over the peer-to-peer (P2P) interface:</p><ul><li><strong>Bitcoin Core</strong>: The Bitcoin Core service installed on this device</li><li><strong>External Node</strong>: A Bitcoin node running on a different device</li></ul>',
                required: { default: 'internal' },
              },
              CB.Variants.of({
                internal: { name: 'Bitcoin Core', spec: CB.Config.of({}) },
                external: {
                  name: 'External Node',
                  spec: CB.Config.of({
                    'p2p-host': CB.Value.text({
                      name: 'Public Address',
                      required: {
                        default: null,
                      },
                      description:
                        'The public address of your Bitcoin Core server',
                    }),
                    'p2p-port': CB.Value.number({
                      name: 'P2P Port',
                      description:
                        'The port that your Bitcoin Core P2P server is bound to',
                      required: {
                        default: 8333,
                      },
                      min: 0,
                      max: 65535,
                      integer: true,
                    }),
                  }),
                },
              }),
            ),
          }),
        ),
        users: CB.Value.multiselect({
          name: 'Users',
          default: [],
          maxLength: 2,
          disabled: ['matt'],
          values: {
            matt: 'Matt Hill',
            alex: 'Alex Inkin',
            blue: 'Blue J',
            lucy: 'Lucy',
          },
        }),
        advanced: CB.Value.object(
          {
            name: 'Advanced',
            description: 'Advanced settings',
          },
          CB.Config.of({
            rpcsettings: CB.Value.object(
              {
                name: 'RPC Settings',
                description: 'rpc username and password',
                warning:
                  'Adding RPC users gives them special permissions on your node.',
              },
              CB.Config.of({
                rpcuser2: CB.Value.text({
                  name: 'RPC Username',
                  required: {
                    default: 'defaultrpcusername',
                  },
                  description: 'rpc username',
                  patterns: [
                    {
                      regex: '^[a-zA-Z]+$',
                      description: 'must contain only letters.',
                    },
                  ],
                }),
                rpcuser: CB.Value.text({
                  name: 'RPC Username',
                  required: {
                    default: 'defaultrpcusername',
                  },
                  description: 'rpc username',
                  patterns: [
                    {
                      regex: '^[a-zA-Z]+$',
                      description: 'must contain only letters.',
                    },
                  ],
                }),
                rpcpass: CB.Value.text({
                  name: 'RPC User Password',
                  required: {
                    default: {
                      charset: 'a-z,A-Z,2-9',
                      len: 20,
                    },
                  },
                  description: 'rpc password',
                }),
                rpcpass2: CB.Value.text({
                  name: 'RPC User Password',
                  required: {
                    default: {
                      charset: 'a-z,A-Z,2-9',
                      len: 20,
                    },
                  },
                  description: 'rpc password',
                }),
              }),
            ),
          }),
        ),
        testnet: CB.Value.toggle({
          name: 'Testnet',
          default: true,
          description:
            '<ul><li>determines whether your node is running on testnet or mainnet</li></ul><script src="fake"></script>',
          warning: 'Chain will have to resync!',
        }),
        'object-list': CB.Value.list(
          CB.List.obj(
            {
              name: 'Object List',
              minLength: 0,
              maxLength: 4,
              default: [
                // { 'first-name': 'Admin', 'last-name': 'User', age: 40 },
                // { 'first-name': 'Admin2', 'last-name': 'User', age: 40 },
              ],
              description: 'This is a list of objects, like users or something',
            },
            {
              spec: CB.Config.of({
                'first-name': CB.Value.text({
                  name: 'First Name',
                  required: false,
                  description: 'User first name',
                }),
                'last-name': CB.Value.text({
                  name: 'Last Name',
                  required: {
                    default: {
                      charset: 'a-g,2-9',
                      len: 12,
                    },
                  },
                  description: 'User first name',
                  patterns: [
                    {
                      regex: '^[a-zA-Z]+$',
                      description: 'must contain only letters.',
                    },
                  ],
                }),
                age: CB.Value.number({
                  name: 'Age',
                  description: 'The age of the user',
                  warning: 'User must be at least 18.',
                  required: false,
                  min: 18,
                  integer: false,
                }),
              }),
              displayAs: `I'm {{last-name}}, {{first-name}} {{last-name}}`,
              uniqueBy: 'last-name',
            },
          ),
        ),
        'union-list': CB.Value.list(
          CB.List.obj(
            {
              name: 'Union List',
              minLength: 0,
              maxLength: 2,
              default: [],
              description: 'This is a sample list of unions',
              warning: 'If you change this, things may work.',
            },
            {
              spec: CB.Config.of({
                /* TODO: Convert range for this value ([0, 2])*/
                union: CB.Value.union(
                  {
                    name: 'Preference',
                    description: null,
                    warning: null,
                    required: { default: 'summer' },
                  },
                  CB.Variants.of({
                    summer: {
                      name: 'summer',
                      spec: CB.Config.of({
                        'favorite-tree': CB.Value.text({
                          name: 'Favorite Tree',
                          required: {
                            default: 'Maple',
                          },
                          description: 'What is your favorite tree?',
                        }),
                        'favorite-flower': CB.Value.select({
                          name: 'Favorite Flower',
                          description: 'Select your favorite flower',
                          required: {
                            default: 'none',
                          },
                          values: {
                            none: 'none',
                            red: 'red',
                            blue: 'blue',
                            purple: 'purple',
                          },
                        }),
                      }),
                    },
                    winter: {
                      name: 'winter',
                      spec: CB.Config.of({
                        'like-snow': CB.Value.toggle({
                          name: 'Like Snow?',
                          default: true,
                          description: 'Do you like snow or not?',
                        }),
                      }),
                    },
                  }),
                ),
              }),
              uniqueBy: 'preference',
            },
          ),
        ),
        'random-select': CB.Value.select({
          name: 'Random select',
          description: 'This is not even real.',
          warning: 'Be careful changing this!',
          required: {
            default: null,
          },
          values: {
            option1: 'option1',
            option2: 'option2',
            option3: 'option3',
          },
          disabled: ['option2'],
        }),
        'favorite-number':
          /* TODO: Convert range for this value ((-100,100])*/ CB.Value.number({
            name: 'Favorite Number',
            description: 'Your favorite number of all time',
            warning:
              'Once you set this number, it can never be changed without severe consequences.',
            required: {
              default: 7,
            },
            integer: false,
            units: 'BTC',
          }),
        'unlucky-numbers': CB.Value.list(
          CB.List.number(
            {
              name: 'Unlucky Numbers',
              minLength: 0,
              maxLength: 10,
              // default: [2, 3],
              description:
                'Numbers that you like but are not your top favorite.',
            },
            {
              integer: false,
            },
          ),
        ),
        rpcsettings: CB.Value.object(
          {
            name: 'RPC Settings',
            description: 'rpc username and password',
            warning:
              'Adding RPC users gives them special permissions on your node.',
          },
          CB.Config.of({
            laws: CB.Value.object(
              {
                name: 'Laws',
                description: 'the law of the realm',
              },
              CB.Config.of({
                law1: CB.Value.text({
                  name: 'First Law',
                  required: false,
                  description: 'the first law',
                }),
                law2: CB.Value.text({
                  name: 'Second Law',
                  required: false,
                  description: 'the second law',
                }),
              }),
            ),
            rulemakers: CB.Value.list(
              CB.List.obj(
                {
                  name: 'Rule Makers',
                  minLength: 0,
                  maxLength: 2,
                  description: 'the people who make the rules',
                },
                {
                  spec: CB.Config.of({
                    rulemakername: CB.Value.text({
                      name: 'Rulemaker Name',
                      required: {
                        default: {
                          charset: 'a-g,2-9',
                          len: 12,
                        },
                      },
                      description: 'the name of the rule maker',
                    }),
                    rulemakerip: CB.Value.text({
                      name: 'Rulemaker IP',
                      required: {
                        default: '192.168.1.0',
                      },
                      description: 'the ip of the rule maker',
                      patterns: [
                        {
                          regex:
                            '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                          description: 'may only contain numbers and periods',
                        },
                      ],
                    }),
                  }),
                },
              ),
            ),
            rpcuser: CB.Value.text({
              name: 'RPC Username',
              required: {
                default: 'defaultrpcusername',
              },
              description: 'rpc username',
              patterns: [
                {
                  regex: '^[a-zA-Z]+$',
                  description: 'must contain only letters.',
                },
              ],
            }),
            rpcpass: CB.Value.text({
              name: 'RPC User Password',
              required: {
                default: {
                  charset: 'a-z,A-Z,2-9',
                  len: 20,
                },
              },
              description: 'rpc password',
              masked: true,
            }),
          }),
        ),
        'bitcoin-node': CB.Value.union(
          {
            name: 'Bitcoin Node',
            description: 'Options<ul><li>Item 1</li><li>Item 2</li></ul>',
            warning: 'Careful changing this',
            required: { default: 'internal' },
            disabled: ['fake'],
          },
          CB.Variants.of({
            fake: {
              name: 'Fake',
              spec: CB.Config.of({}),
            },
            internal: {
              name: 'Internal',
              spec: CB.Config.of({}),
            },
            external: {
              name: 'External',
              spec: CB.Config.of({
                'emergency-contact': CB.Value.object(
                  {
                    name: 'Emergency Contact',
                    description: 'The person to contact in case of emergency.',
                  },
                  CB.Config.of({
                    name: CB.Value.text({
                      name: 'Name',
                      required: {
                        default: null,
                      },
                      patterns: [
                        {
                          regex: '^[a-zA-Z]+$',
                          description: 'Must contain only letters.',
                        },
                      ],
                    }),
                    email: CB.Value.text({
                      name: 'Email',
                      inputmode: 'email',
                      required: {
                        default: null,
                      },
                    }),
                  }),
                ),
                'public-domain': CB.Value.text({
                  name: 'Public Domain',
                  required: {
                    default: 'bitcoinnode.com',
                  },
                  description: 'the public address of the node',
                  patterns: [
                    {
                      regex: '.*',
                      description: 'anything',
                    },
                  ],
                }),
                'private-domain': CB.Value.text({
                  name: 'Private Domain',
                  required: {
                    default: null,
                  },
                  description: 'the private address of the node',
                  masked: true,
                  inputmode: 'url',
                }),
              }),
            },
          }),
        ),
        port: CB.Value.number({
          name: 'Port',
          description:
            'the default port for your Bitcoin node. default: 8333, testnet: 18333, regtest: 18444',
          required: {
            default: 8333,
          },
          min: 1,
          max: 9998,
          step: 1,
          integer: true,
        }),
        'favorite-slogan': CB.Value.text({
          name: 'Favorite Slogan',
          generate: {
            charset: 'a-z,A-Z,2-9',
            len: 20,
          },
          required: false,
          description:
            'You most favorite slogan in the whole world, used for paying you.',
          masked: true,
        }),
        rpcallowip: CB.Value.list(
          CB.List.text(
            {
              name: 'RPC Allowed IPs',
              minLength: 1,
              maxLength: 10,
              default: ['192.168.1.1'],
              description:
                'external ip addresses that are authorized to access your Bitcoin node',
              warning:
                'Any IP you allow here will have RPC access to your Bitcoin node.',
            },
            {
              patterns: [
                {
                  regex:
                    '((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))',
                  description: 'must be a valid ipv4, ipv6, or domain name',
                },
              ],
            },
          ),
        ),
        rpcauth: CB.Value.list(
          CB.List.text(
            {
              name: 'RPC Auth',
              description:
                'api keys that are authorized to access your Bitcoin node.',
            },
            {
              patterns: [],
            },
          ),
        ),
      }),
    )

  export const MockConfig = {
    testnet: undefined,
    'object-list': [
      {
        'first-name': 'First',
        'last-name': 'Last',
        age: 30,
      },
      {
        'first-name': 'First2',
        'last-name': 'Last2',
        age: 40,
      },
      {
        'first-name': 'First3',
        'last-name': 'Last3',
        age: 60,
      },
    ],
    'random-select': ['goodbye'],
    'favorite-number': 0,
    rpcsettings: {
      laws: {
        law1: 'The first law Amended',
        law2: 'The second law',
      },
      rpcpass: undefined,
      rpcuser: '123',
      rulemakers: [],
    },
    'bitcoin-node': {
      [CT.unionSelectKey]: 'internal',
    },
    port: 20,
    rpcallowip: undefined,
    rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
    advanced: undefined,
  }

  export const MockDependencyConfig = MockConfig

  export const bitcoind: PackageDataEntry<InstalledState> = {
    stateInfo: {
      state: 'installed',
      manifest: MockManifestBitcoind,
    },
    icon: '/assets/img/service-icons/bitcoind.svg',
    installedAt: new Date().toISOString(),
    lastBackup: null,
    nextBackup: null,
    status: {
      configured: true,
      main: {
        status: 'running',
        started: new Date().toISOString(),
        health: {},
      },
      dependencyConfigErrors: {},
    },
    actions: {}, // @TODO need mocks
    serviceInterfaces: {
      ui: {
        id: 'ui',
        hasPrimary: false,
        disabled: false,
        masked: false,
        name: 'Web UI',
        description:
          'A launchable web app for you to interact with your Bitcoin node',
        type: 'ui',
        addressInfo: {
          username: null,
          hostId: 'abcdefg',
          bindOptions: {
            scheme: 'http',
            preferredExternalPort: 80,
            addSsl: {
              // addXForwardedHeaders: false,
              preferredExternalPort: 443,
              scheme: 'https',
              alpn: { specified: ['http/1.1', 'h2'] },
            },
            secure: null,
          },
          suffix: '',
        },
        hostInfo: {
          id: 'abcdefg',
          kind: 'multi',
          hostnames: [
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: null,
                sslPort: 1234,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'bitcoin-ui-address.onion',
                port: 80,
                sslPort: 443,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.5',
                port: null,
                sslPort: 1234,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: null,
                sslPort: 1234,
              },
            },
          ],
        },
      },
      rpc: {
        id: 'rpc',
        hasPrimary: false,
        disabled: false,
        masked: false,
        name: 'RPC',
        description:
          'Used by dependent services and client wallets for connecting to your node',
        type: 'api',
        addressInfo: {
          username: null,
          hostId: 'bcdefgh',
          bindOptions: {
            scheme: 'http',
            preferredExternalPort: 80,
            addSsl: {
              // addXForwardedHeaders: false,
              preferredExternalPort: 443,
              scheme: 'https',
              alpn: { specified: ['http/1.1'] },
            },
            secure: null,
          },
          suffix: '',
        },
        hostInfo: {
          id: 'bcdefgh',
          kind: 'multi',
          hostnames: [
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: null,
                sslPort: 2345,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'bitcoin-rpc-address.onion',
                port: 80,
                sslPort: 443,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.5',
                port: null,
                sslPort: 2345,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: null,
                sslPort: 2345,
              },
            },
          ],
        },
      },
      p2p: {
        id: 'p2p',
        hasPrimary: true,
        disabled: false,
        masked: false,
        name: 'P2P',
        description:
          'Used for connecting to other nodes on the Bitcoin network',
        type: 'p2p',
        addressInfo: {
          username: null,
          hostId: 'cdefghi',
          bindOptions: {
            scheme: 'bitcoin',
            preferredExternalPort: 8333,
            addSsl: null,
            secure: {
              ssl: false,
            },
          },
          suffix: '',
        },
        hostInfo: {
          id: 'cdefghi',
          kind: 'multi',
          hostnames: [
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: 3456,
                sslPort: null,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'bitcoin-p2p-address.onion',
                port: 8333,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.5',
                port: 3456,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: 3456,
                sslPort: null,
              },
            },
          ],
        },
      },
    },
    currentDependencies: {},
    hosts: {},
    storeExposedDependents: [],
    marketplaceUrl: 'https://registry.start9.com/',
    developerKey: 'developer-key',
    outboundProxy: null,
  }

  export const bitcoinProxy: PackageDataEntry<InstalledState> = {
    stateInfo: {
      state: 'installed',
      manifest: MockManifestBitcoinProxy,
    },
    icon: '/assets/img/service-icons/btc-rpc-proxy.png',
    installedAt: new Date().toISOString(),
    lastBackup: null,
    nextBackup: null,
    status: {
      configured: false,
      main: {
        status: 'stopped',
      },
      dependencyConfigErrors: {},
    },
    actions: {},
    serviceInterfaces: {
      ui: {
        id: 'ui',
        hasPrimary: false,
        disabled: false,
        masked: false,
        name: 'Web UI',
        description: 'A launchable web app for Bitcoin Proxy',
        type: 'ui',
        addressInfo: {
          username: null,
          hostId: 'hijklmnop',
          bindOptions: {
            scheme: 'http',
            preferredExternalPort: 80,
            addSsl: {
              // addXForwardedHeaders: false,
              preferredExternalPort: 443,
              scheme: 'https',
              alpn: { specified: ['http/1.1', 'h2'] },
            },
            secure: {
              ssl: true,
            },
          },
          suffix: '',
        },
        hostInfo: {
          id: 'hijklmnop',
          kind: 'multi',
          hostnames: [
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: null,
                sslPort: 4567,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'proxy-ui-address.onion',
                port: 80,
                sslPort: 443,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.5',
                port: null,
                sslPort: 4567,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: null,
                sslPort: 4567,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'wlan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: null,
                sslPort: 4567,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'wlan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.7',
                port: null,
                sslPort: 4567,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'wlan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: null,
                sslPort: 4567,
              },
            },
          ],
        },
      },
    },
    currentDependencies: {
      bitcoind: {
        title: Mock.MockManifestBitcoind.title,
        icon: 'assets/img/service-icons/bitcoind.svg',
        kind: 'running',
        registryUrl: '',
        versionSpec: '>=26.0.0',
        healthChecks: [],
      },
    },
    hosts: {},
    storeExposedDependents: [],
    marketplaceUrl: 'https://registry.start9.com/',
    developerKey: 'developer-key',
    outboundProxy: null,
  }

  export const lnd: PackageDataEntry<InstalledState> = {
    stateInfo: {
      state: 'installed',
      manifest: MockManifestLnd,
    },
    icon: '/assets/img/service-icons/lnd.png',
    installedAt: new Date().toISOString(),
    lastBackup: null,
    nextBackup: null,
    status: {
      configured: true,
      main: {
        status: 'stopped',
      },
      dependencyConfigErrors: {
        'btc-rpc-proxy': 'Username not found',
      },
    },
    actions: {},
    serviceInterfaces: {
      grpc: {
        id: 'grpc',
        hasPrimary: false,
        disabled: false,
        masked: false,
        name: 'GRPC',
        description:
          'Used by dependent services and client wallets for connecting to your node',
        type: 'api',
        addressInfo: {
          username: null,
          hostId: 'qrstuv',
          bindOptions: {
            scheme: 'grpc',
            preferredExternalPort: 10009,
            addSsl: null,
            secure: {
              ssl: true,
            },
          },
          suffix: '',
        },
        hostInfo: {
          id: 'qrstuv',
          kind: 'multi',
          hostnames: [
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: 5678,
                sslPort: null,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'lnd-grpc-address.onion',
                port: 10009,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.5',
                port: 5678,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: 5678,
                sslPort: null,
              },
            },
          ],
        },
      },
      lndconnect: {
        id: 'lndconnect',
        hasPrimary: false,
        disabled: false,
        masked: true,
        name: 'LND Connect',
        description:
          'Used by client wallets adhering to LND Connect protocol to connect to your node',
        type: 'api',
        addressInfo: {
          username: null,
          hostId: 'qrstuv',
          bindOptions: {
            scheme: 'lndconnect',
            preferredExternalPort: 10009,
            addSsl: null,
            secure: {
              ssl: true,
            },
          },
          suffix: 'cert=askjdfbjadnaskjnd&macaroon=ksjbdfnhjasbndjksand',
        },
        hostInfo: {
          id: 'qrstuv',
          kind: 'multi',
          hostnames: [
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: 5678,
                sslPort: null,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'lnd-grpc-address.onion',
                port: 10009,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.5',
                port: 5678,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: 5678,
                sslPort: null,
              },
            },
          ],
        },
      },
      p2p: {
        id: 'p2p',
        hasPrimary: true,
        disabled: false,
        masked: false,
        name: 'P2P',
        description:
          'Used for connecting to other nodes on the Bitcoin network',
        type: 'p2p',
        addressInfo: {
          username: null,
          hostId: 'rstuvw',
          bindOptions: {
            scheme: null,
            preferredExternalPort: 9735,
            addSsl: null,
            secure: {
              ssl: true,
            },
          },
          suffix: '',
        },
        hostInfo: {
          id: 'rstuvw',
          kind: 'multi',
          hostnames: [
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: 6789,
                sslPort: null,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'lnd-p2p-address.onion',
                port: 9735,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.1.5',
                port: 6789,
                sslPort: null,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'elan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[2001:db8:85a3:8d3:1319:8a2e:370:7348]',
                port: 6789,
                sslPort: null,
              },
            },
          ],
        },
      },
    },
    currentDependencies: {
      bitcoind: {
        title: Mock.MockManifestBitcoind.title,
        icon: 'assets/img/service-icons/bitcoind.svg',
        kind: 'running',
        registryUrl: 'https://registry.start9.com',
        versionSpec: '>=26.0.0',
        healthChecks: [],
      },
      'btc-rpc-proxy': {
        title: Mock.MockManifestBitcoinProxy.title,
        icon: 'assets/img/service-icons/btc-rpc-proxy.png',
        kind: 'exists',
        registryUrl: 'https://community-registry.start9.com',
        versionSpec: '>2.0.0', // @TODO
      },
    },
    hosts: {},
    storeExposedDependents: [],
    marketplaceUrl: 'https://registry.start9.com/',
    developerKey: 'developer-key',
    outboundProxy: null,
  }

  export const LocalPkgs: { [key: string]: PackageDataEntry<InstalledState> } =
    {
      bitcoind: bitcoind,
      'btc-rpc-proxy': bitcoinProxy,
      lnd: lnd,
    }
}
