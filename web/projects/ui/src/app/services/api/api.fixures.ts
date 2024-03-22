import {
  InstalledState,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
  ServerStatusInfo,
} from 'src/app/services/patch-db/data-model'
import {
  Metrics,
  NotificationLevel,
  RR,
  ServerNotifications,
} from './api.types'
import { BTC_ICON, LND_ICON, PROXY_ICON } from './api-icons'
import {
  DependencyMetadata,
  Manifest,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { Log } from '@start9labs/shared'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { Config } from '@start9labs/start-sdk/cjs/sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/cjs/sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/cjs/sdk/lib/config/builder/variants'
import { List } from '@start9labs/start-sdk/cjs/sdk/lib/config/builder/list'
import { unionSelectKey } from '@start9labs/start-sdk/cjs/sdk/lib/config/configTypes'

export module Mock {
  export const ServerUpdated: ServerStatusInfo = {
    'current-backup': null,
    'update-progress': null,
    updated: true,
    restarting: false,
    'shutting-down': false,
  }
  export const MarketplaceEos: RR.GetMarketplaceEosRes = {
    version: '0.3.5.1',
    headline: 'Our biggest release ever.',
    'release-notes': {
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

  export const MockManifestBitcoind: Manifest = {
    id: 'bitcoind',
    title: 'Bitcoin Core',
    version: '0.21.0',
    'git-hash': 'abcdefgh',
    description: {
      short: 'A Bitcoin full node by Bitcoin Core.',
      long: 'Bitcoin is a decentralized consensus protocol and settlement network.',
    },
    replaces: ['banks', 'governments'],
    'release-notes': 'Taproot, Schnorr, and more.',
    license: 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/bitcoind-wrapper',
    'upstream-repo': 'https://github.com/bitcoin/bitcoin',
    'support-site': 'https://bitcoin.org',
    'marketing-site': 'https://bitcoin.org',
    'donation-url': 'https://start9.com',
    alerts: {
      install: 'Bitcoin can take over a week to sync.',
      uninstall:
        'Chain state will be lost, as will any funds stored on your Bitcoin Core waller that have not been backed up.',
      restore: null,
      start: 'Starting Bitcoin is good for your health.',
      stop: null,
    },
    'os-version': '0.2.12',
    dependencies: {},
    'has-config': true,
  }

  export const MockManifestLnd: Manifest = {
    id: 'lnd',
    title: 'Lightning Network Daemon',
    version: '0.11.1',
    description: {
      short: 'A bolt spec compliant client.',
      long: 'More info about LND. More info about LND. More info about LND.',
    },
    'release-notes': 'Dual funded channels!',
    license: 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/lnd-wrapper',
    'upstream-repo': 'https://github.com/lightningnetwork/lnd',
    'support-site': 'https://lightning.engineering/',
    'marketing-site': 'https://lightning.engineering/',
    'donation-url': null,
    alerts: {
      install: null,
      uninstall: null,
      restore:
        'If this is a duplicate instance of the same LND node, you may loose your funds.',
      start: 'Starting LND is good for your health.',
      stop: null,
    },
    'os-version': '0.2.12',
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
    'has-config': true,
  }

  export const MockManifestBitcoinProxy: Manifest = {
    id: 'btc-rpc-proxy',
    title: 'Bitcoin Proxy',
    version: '0.2.2',
    'git-hash': 'lmnopqrx',
    description: {
      short: 'A super charger for your Bitcoin node.',
      long: 'More info about Bitcoin Proxy. More info about Bitcoin Proxy. More info about Bitcoin Proxy.',
    },
    'release-notes': 'Even better support for Bitcoin and wallets!',
    license: 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/btc-rpc-proxy-wrapper',
    'upstream-repo': 'https://github.com/Kixunil/btc-rpc-proxy',
    'support-site': '',
    'marketing-site': '',
    'donation-url': 'https://start9.com',
    alerts: {
      install: 'Testing install alert',
      uninstall: null,
      restore: null,
      start: null,
      stop: null,
    },
    'os-version': '0.2.12',
    dependencies: {
      bitcoind: {
        description: 'Bitcoin Proxy requires a Bitcoin node.',
        optional: false,
      },
    },
    'has-config': false,
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
        'dependency-metadata': {},
        'published-at': new Date().toISOString(),
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
        'dependency-metadata': {},
        'published-at': new Date().toISOString(),
      },
      '0.21.0': {
        icon: BTC_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.21.0',
          'release-notes':
            'For a complete list of changes, please visit <a href="https://bitcoincore.org/en/releases/0.21.0/">https://bitcoincore.org/en/releases/0.21.0/</a><br /><ul><li>Taproot!</li><li>New RPCs</li><li>Experimental Descriptor Wallets</li></ul>',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
        'published-at': new Date().toISOString(),
      },
      latest: {
        icon: BTC_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        screenshots: ['one.png', 'two.png', 'three.png'],
        manifest: {
          ...Mock.MockManifestBitcoind,
          'release-notes':
            'For a complete list of changes, please visit <a href="https://bitcoincore.org/en/releases/0.21.0/" target="_blank">https://bitcoincore.org/en/releases/0.21.0/</a><br />Or in [markdown](https://bitcoincore.org/en/releases/0.21.0/)<ul><li>Taproot!</li><li>New RPCs</li><li>Experimental Descriptor Wallets</li></ul>',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
        'published-at': new Date().toISOString(),
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
          'release-notes': 'release notes for LND 0.11.0',
        },
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          bitcoind: BitcoinDep,
          'btc-rpc-proxy': ProxyDep,
        },
        'published-at': new Date().toISOString(),
      },
      '0.11.1': {
        icon: LND_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.1',
          'release-notes': 'release notes for LND 0.11.1',
        },
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          bitcoind: BitcoinDep,
          'btc-rpc-proxy': ProxyDep,
        },
        'published-at': new Date().toISOString(),
      },
      latest: {
        icon: LND_ICON,
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestLnd,
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          bitcoind: BitcoinDep,
          'btc-rpc-proxy': ProxyDep,
        },
        'published-at': new Date(new Date().valueOf() + 10).toISOString(),
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
        'dependency-metadata': {
          bitcoind: BitcoinDep,
        },
        'published-at': new Date().toISOString(),
      },
    },
  }

  export const MarketplacePkgsList: RR.GetMarketplacePackagesRes =
    Object.values(Mock.MarketplacePkgs).map(service => service['latest'])

  export const Notifications: ServerNotifications = [
    {
      id: 1,
      'package-id': null,
      'created-at': '2019-12-26T14:20:30.872Z',
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
      'package-id': null,
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 2,
      level: NotificationLevel.Warning,
      title: 'SSH Key Added',
      message: 'A new SSH key was added. If you did not do this, shit is bad.',
      data: null,
      read: false,
    },
    {
      id: 3,
      'package-id': null,
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 3,
      level: NotificationLevel.Info,
      title: 'SSH Key Removed',
      message: 'A SSH key was removed.',
      data: null,
      read: false,
    },
    {
      id: 4,
      'package-id': 'bitcoind',
      'created-at': '2019-12-26T14:20:30.872Z',
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
        'percentage-used': {
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
        'zram-total': {
          value: '7992.00',
          unit: 'MiB',
        },
        'zram-available': {
          value: '7882.50',
          unit: 'MiB',
        },
        'zram-used': {
          value: '109.50',
          unit: 'MiB',
        },
      },
      cpu: {
        'percentage-used': {
          value: '8.4',
          unit: '%',
        },
        'user-space': {
          value: '7.0',
          unit: '%',
        },
        'kernel-space': {
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
        'percentage-used': {
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
        'last-active': '2021-07-14T20:49:17.774Z',
        'user-agent': 'AppleWebKit/{WebKit Rev} (KHTML, like Gecko)',
        metadata: {
          platforms: ['iphone', 'mobileweb', 'mobile', 'ios'],
        },
      },
      klndsfjhbwsajkdnaksj: {
        'last-active': '2019-07-14T20:49:17.774Z',
        'user-agent': 'AppleWebKit/{WebKit Rev} (KHTML, like Gecko)',
        metadata: {
          platforms: ['cli'],
        },
      },
      b7b1a9cef4284f00af9e9dda6e676177: {
        'last-active': '2021-06-14T20:49:17.774Z',
        'user-agent':
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
      'created-at': new Date().toISOString(),
      alg: 'ed25519',
      hostname: 'Matt Key',
      fingerprint: '28:d2:7e:78:61:b4:bf:g2:de:24:15:96:4e:d4:15:53',
    },
    {
      'created-at': new Date().toISOString(),
      alg: 'ed25519',
      hostname: 'Aiden Key',
      fingerprint: '12:f8:7e:78:61:b4:bf:e2:de:24:15:96:4e:d4:72:53',
    },
  ]

  export const SshKey: RR.AddSSHKeyRes = {
    'created-at': new Date().toISOString(),
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
    'available-wifi': [
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
    'unknown-disks': [
      {
        logicalname: 'sbc4',
        label: 'My Backup Drive',
        capacity: 2000000000000,
        used: 100000000000,
        model: 'T7',
        vendor: 'Samsung',
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
        'embassy-os': {
          version: '0.3.0',
          full: true,
          'password-hash':
            // password is asdfasdf
            '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
          'wrapped-key': '',
        },
      },
      {
        id: 'ftcvewdnkemfksdm',
        type: 'cloud',
        name: 'Dropbox 1',
        provider: 'dropbox',
        path: '/Home/backups',
        mountable: true,
        'embassy-os': null,
      },
      {
        id: 'csgashbdjkasnd',
        type: 'cifs',
        name: 'Network Folder 2',
        hostname: 'smb://192.169.10.0',
        path: '/Desktop/embassy-backups-2',
        username: 'TestUser',
        mountable: true,
        'embassy-os': null,
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
        'embassy-os': {
          version: '0.3.0',
          full: true,
          // password is asdfasdf
          'password-hash':
            '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
          'wrapped-key': '',
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
      'package-ids': ['bitcoind', 'lnd'],
    },
    {
      id: 'hahahahaha-mwmwmwmwmwmw',
      name: 'Another Backup Job',
      target: BackupTargets.saved[1],
      cron: '0 * * * *',
      'package-ids': ['lnd'],
    },
  ]

  export const BackupRuns: RR.GetBackupRunsRes = [
    {
      id: 'kladhbfweubdsk',
      'started-at': new Date().toISOString(),
      'completed-at': new Date(new Date().valueOf() + 10000).toISOString(),
      'package-ids': ['bitcoind', 'lnd'],
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
      'started-at': new Date().toISOString(),
      'completed-at': new Date(new Date().valueOf() + 10000).toISOString(),
      'package-ids': ['bitcoind', 'lnd'],
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
    'package-backups': {
      bitcoind: {
        title: 'Bitcoin Core',
        version: '0.21.0',
        'os-version': '0.3.0',
        timestamp: new Date().toISOString(),
      },
      'btc-rpc-proxy': {
        title: 'Bitcoin Proxy',
        version: '0.2.2',
        'os-version': '0.3.0',
        timestamp: new Date().toISOString(),
      },
    },
  }

  export const getInputSpec = async (): Promise<
    RR.GetPackageConfigRes['spec']
  > =>
    configBuilderToSpec(
      Config.of({
        bitcoin: Value.object(
          {
            name: 'Bitcoin Settings',
            description:
              'RPC and P2P interface configuration options for Bitcoin Core',
          },
          Config.of({
            'bitcoind-p2p': Value.union(
              {
                name: 'P2P Settings',
                description:
                  '<p>The Bitcoin Core node to connect to over the peer-to-peer (P2P) interface:</p><ul><li><strong>Bitcoin Core</strong>: The Bitcoin Core service installed on this device</li><li><strong>External Node</strong>: A Bitcoin node running on a different device</li></ul>',
                required: { default: 'internal' },
              },
              Variants.of({
                internal: { name: 'Bitcoin Core', spec: Config.of({}) },
                external: {
                  name: 'External Node',
                  spec: Config.of({
                    'p2p-host': Value.text({
                      name: 'Public Address',
                      required: {
                        default: null,
                      },
                      description:
                        'The public address of your Bitcoin Core server',
                    }),
                    'p2p-port': Value.number({
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
        users: Value.multiselect({
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
        advanced: Value.object(
          {
            name: 'Advanced',
            description: 'Advanced settings',
          },
          Config.of({
            rpcsettings: Value.object(
              {
                name: 'RPC Settings',
                description: 'rpc username and password',
                warning:
                  'Adding RPC users gives them special permissions on your node.',
              },
              Config.of({
                rpcuser2: Value.text({
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
                rpcuser: Value.text({
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
                rpcpass: Value.text({
                  name: 'RPC User Password',
                  required: {
                    default: {
                      charset: 'a-z,A-Z,2-9',
                      len: 20,
                    },
                  },
                  description: 'rpc password',
                }),
                rpcpass2: Value.text({
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
        testnet: Value.toggle({
          name: 'Testnet',
          default: true,
          description:
            '<ul><li>determines whether your node is running on testnet or mainnet</li></ul><script src="fake"></script>',
          warning: 'Chain will have to resync!',
        }),
        'object-list': Value.list(
          List.obj(
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
              spec: Config.of({
                'first-name': Value.text({
                  name: 'First Name',
                  required: false,
                  description: 'User first name',
                }),
                'last-name': Value.text({
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
                age: Value.number({
                  name: 'Age',
                  description: 'The age of the user',
                  warning: 'User must be at least 18.',
                  required: false,
                  min: 18,
                  integer: false,
                }),
              }),
              displayAs: "I'm {{last-name}}, {{first-name}} {{last-name}}",
              uniqueBy: 'last-name',
            },
          ),
        ),
        'union-list': Value.list(
          List.obj(
            {
              name: 'Union List',
              minLength: 0,
              maxLength: 2,
              default: [],
              description: 'This is a sample list of unions',
              warning: 'If you change this, things may work.',
            },
            {
              spec: Config.of({
                /* TODO: Convert range for this value ([0, 2])*/
                union: Value.union(
                  {
                    name: 'Preference',
                    description: null,
                    warning: null,
                    required: { default: 'summer' },
                  },
                  Variants.of({
                    summer: {
                      name: 'summer',
                      spec: Config.of({
                        'favorite-tree': Value.text({
                          name: 'Favorite Tree',
                          required: {
                            default: 'Maple',
                          },
                          description: 'What is your favorite tree?',
                        }),
                        'favorite-flower': Value.select({
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
                      spec: Config.of({
                        'like-snow': Value.toggle({
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
        'random-select': Value.select({
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
          /* TODO: Convert range for this value ((-100,100])*/ Value.number({
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
        'unlucky-numbers': Value.list(
          List.number(
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
        rpcsettings: Value.object(
          {
            name: 'RPC Settings',
            description: 'rpc username and password',
            warning:
              'Adding RPC users gives them special permissions on your node.',
          },
          Config.of({
            laws: Value.object(
              {
                name: 'Laws',
                description: 'the law of the realm',
              },
              Config.of({
                law1: Value.text({
                  name: 'First Law',
                  required: false,
                  description: 'the first law',
                }),
                law2: Value.text({
                  name: 'Second Law',
                  required: false,
                  description: 'the second law',
                }),
              }),
            ),
            rulemakers: Value.list(
              List.obj(
                {
                  name: 'Rule Makers',
                  minLength: 0,
                  maxLength: 2,
                  description: 'the people who make the rules',
                },
                {
                  spec: Config.of({
                    rulemakername: Value.text({
                      name: 'Rulemaker Name',
                      required: {
                        default: {
                          charset: 'a-g,2-9',
                          len: 12,
                        },
                      },
                      description: 'the name of the rule maker',
                    }),
                    rulemakerip: Value.text({
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
            rpcuser: Value.text({
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
            rpcpass: Value.text({
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
        'bitcoin-node': Value.union(
          {
            name: 'Bitcoin Node',
            description: 'Options<ul><li>Item 1</li><li>Item 2</li></ul>',
            warning: 'Careful changing this',
            required: { default: 'internal' },
            disabled: ['fake'],
          },
          Variants.of({
            fake: {
              name: 'Fake',
              spec: Config.of({}),
            },
            internal: {
              name: 'Internal',
              spec: Config.of({}),
            },
            external: {
              name: 'External',
              spec: Config.of({
                'emergency-contact': Value.object(
                  {
                    name: 'Emergency Contact',
                    description: 'The person to contact in case of emergency.',
                  },
                  Config.of({
                    name: Value.text({
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
                    email: Value.text({
                      name: 'Email',
                      inputmode: 'email',
                      required: {
                        default: null,
                      },
                    }),
                  }),
                ),
                'public-domain': Value.text({
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
                'private-domain': Value.text({
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
        port: Value.number({
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
        'favorite-slogan': Value.text({
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
        rpcallowip: Value.list(
          List.text(
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
        rpcauth: Value.list(
          List.text(
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
      [unionSelectKey]: 'internal',
    },
    port: 20,
    rpcallowip: undefined,
    rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
    advanced: undefined,
  }

  export const MockDependencyConfig = MockConfig

  export const bitcoind: PackageDataEntry<InstalledState> = {
    'state-info': {
      state: PackageState.Installed,
      manifest: MockManifestBitcoind,
    },
    icon: '/assets/img/service-icons/bitcoind.svg',
    'last-backup': null,
    status: {
      configured: true,
      main: {
        status: PackageMainStatus.Running,
        started: new Date().toISOString(),
        health: {},
      },
      'dependency-config-errors': {},
    },
    actions: {}, // @TODO need mocks
    'service-interfaces': {
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
              addXForwardedHeaders: false,
              preferredExternalPort: 443,
              scheme: 'https',
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
              addXForwardedHeaders: false,
              preferredExternalPort: 443,
              scheme: 'https',
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
    'current-dependencies': {},
    'dependency-info': {},
    'marketplace-url': 'https://registry.start9.com/',
    'developer-key': 'developer-key',
    'has-config': true,
    outboundProxy: null,
  }

  export const bitcoinProxy: PackageDataEntry<InstalledState> = {
    'state-info': {
      state: PackageState.Installed,
      manifest: MockManifestBitcoinProxy,
    },
    icon: '/assets/img/service-icons/btc-rpc-proxy.png',
    'last-backup': null,
    status: {
      configured: false,
      main: {
        status: PackageMainStatus.Stopped,
      },
      'dependency-config-errors': {},
    },
    actions: {},
    'service-interfaces': {
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
              addXForwardedHeaders: false,
              preferredExternalPort: 443,
              scheme: 'https',
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
    'current-dependencies': {
      bitcoind: {
        versionRange: '>=26.0.0',
        'health-checks': [],
      },
    },
    'dependency-info': {
      bitcoind: {
        title: Mock.MockManifestBitcoind.title,
        icon: 'assets/img/service-icons/bitcoind.svg',
      },
    },
    'marketplace-url': 'https://registry.start9.com/',
    'developer-key': 'developer-key',
    'has-config': true,
    outboundProxy: null,
  }

  export const lnd: PackageDataEntry<InstalledState> = {
    'state-info': {
      state: PackageState.Installed,
      manifest: MockManifestLnd,
    },
    icon: '/assets/img/service-icons/lnd.png',
    'last-backup': null,
    status: {
      configured: true,
      main: {
        status: PackageMainStatus.Stopped,
      },
      'dependency-config-errors': {
        'btc-rpc-proxy': 'Username not found',
      },
    },
    actions: {},
    'service-interfaces': {
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
    'current-dependencies': {
      bitcoind: {
        versionRange: '>=26.0.0',
        'health-checks': [],
      },
      'btc-rpc-proxy': {
        versionRange: '>2.0.0', // @TODO
        'health-checks': [],
      },
    },
    'dependency-info': {
      bitcoind: {
        title: Mock.MockManifestBitcoind.title,
        icon: 'assets/img/service-icons/bitcoind.svg',
      },
      'btc-rpc-proxy': {
        title: Mock.MockManifestBitcoinProxy.title,
        icon: 'assets/img/service-icons/btc-rpc-proxy.png',
      },
    },
    'marketplace-url': 'https://registry.start9.com/',
    'developer-key': 'developer-key',
    'has-config': true,
    outboundProxy: null,
  }

  export const LocalPkgs: { [key: string]: PackageDataEntry<InstalledState> } =
    {
      bitcoind: bitcoind,
      'btc-rpc-proxy': bitcoinProxy,
      lnd: lnd,
    }
}
