import {
  DependencyErrorType,
  HealthResult,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
  ServerStatusInfo,
} from 'src/app/services/patch-db/data-model'
import {
  RR,
  NotificationLevel,
  ServerNotifications,
  Metrics,
} from './api.types'

import { BTC_ICON, LND_ICON, PROXY_ICON } from './api-icons'
import {
  DependencyMetadata,
  MarketplacePkg,
  Manifest,
} from '@start9labs/marketplace'
import { Log } from '@start9labs/shared'
// TODO: start-sdk: Figure out why it doesn't work
import { unionSelectKey } from '@start9labs/start-sdk/lib/config/configTypes'
import { List } from '@start9labs/start-sdk/lib/config/builder/list'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { Variants } from '@start9labs/start-sdk/lib/config/builder/variants'
import { Config } from '@start9labs/start-sdk/lib/config/builder/config'

export module Mock {
  export const ServerUpdated: ServerStatusInfo = {
    'current-backup': null,
    'update-progress': null,
    updated: true,
    'shutting-down': false,
  }
  export const MarketplaceEos: RR.GetMarketplaceEosRes = {
    version: '0.3.4.2',
    headline: 'Our biggest release ever.',
    'release-notes': {
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
    assets: {
      icon: 'icon.png',
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
    dependencies: {},
    'os-version': '0.4.0',
  }

  export const MockManifestLnd: Manifest = {
    id: 'lnd',
    title: 'Lightning Network Daemon',
    version: '0.11.1',
    description: {
      short: 'A bolt spec compliant client.',
      long: 'More info about LND. More info about LND. More info about LND.',
    },
    assets: {
      icon: 'icon.png',
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
    dependencies: {
      bitcoind: {
        version: '=0.21.0',
        description: 'LND needs bitcoin to live.',
        requirement: {
          type: 'opt-out',
          how: 'You can use an external node from your server if you prefer.',
        },
      },
      'btc-rpc-proxy': {
        version: '>=0.2.2',
        description:
          'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
        requirement: {
          type: 'opt-in',
          how: `To use Proxy's user management system, go to LND config and select Bitcoin Proxy under Bitcoin config.`,
        },
      },
    },
    'os-version': '0.4.0',
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
    assets: {
      icon: 'icon.png',
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
    dependencies: {
      bitcoind: {
        version: '>=0.20.0',
        description: 'Bitcoin Proxy requires a Bitcoin node.',
        requirement: {
          type: 'required',
        },
      },
    },
    'os-version': '0.4.0',
  }

  export const BitcoinDep: DependencyMetadata = {
    title: 'Bitcoin Core',
    icon: BTC_ICON,
    hidden: true,
  }

  export const ProxyDep: DependencyMetadata = {
    title: 'Bitcoin Proxy',
    icon: PROXY_ICON,
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
        categories: ['bitcoin', 'cryptocurrency'],
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
    },
    {
      id: 4,
      'package-id': 'bitcoind',
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 4,
      level: NotificationLevel.Error,
      title: 'Service Crashed',
      message: new Array(40)
        .fill(
          `2021-11-27T18:36:30.451064Z 2021-11-27T18:36:30Z tor: Thread interrupt
        2021-11-27T18:36:30.452833Z 2021-11-27T18:36:30Z Shutdown: In progress...
        2021-11-27T18:36:30.453128Z 2021-11-27T18:36:30Z addcon thread exit
        2021-11-27T18:36:30.453369Z 2021-11-27T18:36:30Z torcontrol thread exit`,
        )
        .join(''),
      data: null,
    },
  ]

  export function getMetrics(): Metrics {
    return {
      general: {
        temperature: (Math.random() * 100).toFixed(1),
      },
      memory: {
        'percentage-used': '20',
        total: (Math.random() * 100).toFixed(2),
        available: '18000',
        used: '4000',
        'swap-total': '1000',
        'swap-free': Math.random().toFixed(2),
        'swap-used': '0',
      },
      cpu: {
        'user-space': '100',
        'kernel-space': '50',
        'io-wait': String(Math.random() * 50),
        idle: '80',
        usage: '30',
      },
      disk: {
        size: '1000',
        used: '900',
        available: '100',
        'percentage-used': '90',
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

  export const InputSpec: RR.GetPackageConfigRes['spec'] = Config.of({
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
                  description: 'The public address of your Bitcoin Core server',
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
    'random-enum': Value.select({
      name: 'Random Enum',
      description: 'This is not even real.',
      warning: 'Be careful changing this!',
      required: {
        default: 'null',
      },
      values: {
        null: 'null',
        option1: 'option1',
        option2: 'option2',
        option3: 'option3',
      },
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
          description: 'Numbers that you like but are not your top favorite.',
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
      },
      Variants.of({
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
      step: '1',
      integer: true,
    }),
    'favorite-slogan': Value.text({
      name: 'Favorite Slogan',
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
  })

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

  export const bitcoind: PackageDataEntry = {
    state: PackageState.Installed,
    icon: '/assets/img/service-icons/bitcoind.png',
    manifest: MockManifestBitcoind,
    installed: {
      'last-backup': null,
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Running,
          started: '2021-06-14T20:49:17.774Z',
          health: {
            'ephemeral-health-check': {
              name: 'Ephemeral Health Check',
              result: HealthResult.Starting,
            },
            'chain-state': {
              name: 'Chain State',
              result: HealthResult.Loading,
              message: 'Bitcoin is syncing from genesis',
            },
            'p2p-interface': {
              name: 'P2P Interface',
              result: HealthResult.Success,
              message: 'the health check ran successfully',
            },
            'rpc-interface': {
              name: 'RPC Interface',
              result: HealthResult.Failure,
              error: 'RPC interface unreachable.',
            },
            'unnecessary-health-check': {
              name: 'Totally Unnecessary',
              result: HealthResult.Disabled,
              reason: 'You disabled this on purpose',
            },
          },
        },
        'dependency-errors': {},
      },
      'address-info': {
        rpc: {
          name: 'Bitcoin RPC',
          description: `Bitcoin's RPC interface`,
          addresses: [
            'http://bitcoind-rpc-address.onion',
            'https://bitcoind-rpc-address.local',
            'https://192.168.1.1:8332',
          ],
          ui: true,
        },
        p2p: {
          name: 'Bitcoin P2P',
          description: `Bitcoin's P2P interface`,
          addresses: [
            'bitcoin://bitcoind-rpc-address.onion',
            'bitcoin://192.168.1.1:8333',
          ],
          ui: true,
        },
      },
      'current-dependencies': {},
      'dependency-info': {},
      'marketplace-url': 'https://registry.start9.com/',
      'developer-key': 'developer-key',
      'has-config': true,
    },
    actions: {
      resync: {
        name: 'Resync Blockchain',
        description: 'Use this to resync the Bitcoin blockchain from genesis',
        warning: 'This will take a couple of days.',
        disabled: null,
        group: null,
        'input-spec': {
          reason: {
            type: 'text',
            inputmode: 'text',
            name: 'Re-sync Reason',
            description: 'Your reason for re-syncing. Why are you doing this?',
            placeholder: null,
            required: true,
            masked: false,
            minLength: null,
            maxLength: null,
            patterns: [
              {
                regex: '^[a-zA-Z]+$',
                description: 'must contain only letters.',
              },
            ],
            warning: null,
            default: null,
            disabled: false,
            immutable: false,
            generate: null,
          },
        },
      },
    },
  }

  export const bitcoinProxy: PackageDataEntry = {
    state: PackageState.Installed,
    icon: '/assets/img/service-icons/btc-rpc-proxy.png',
    manifest: MockManifestBitcoinProxy,
    installed: {
      'last-backup': null,
      status: {
        configured: false,
        main: {
          status: PackageMainStatus.Stopped,
        },
        'dependency-errors': {},
      },
      'address-info': {
        rpc: {
          name: 'Proxy RPC addresses',
          description: `Use these addresses to access Proxy's RPC interface`,
          addresses: [
            'http://bitcoinproxy-rpc-address.onion',
            'https://bitcoinproxy-rpc-address.local',
          ],
          ui: false,
        },
      },
      'current-dependencies': {
        bitcoind: {
          'health-checks': [],
        },
      },
      'dependency-info': {
        bitcoind: {
          title: 'Bitcoin Core',
          icon: 'assets/img/service-icons/bitcoind.svg',
        },
      },
      'marketplace-url': 'https://registry.start9.com/',
      'developer-key': 'developer-key',
      'has-config': true,
    },
    actions: {},
  }

  export const lnd: PackageDataEntry = {
    state: PackageState.Installed,
    icon: '/assets/img/service-icons/lnd.png',
    manifest: MockManifestLnd,
    installed: {
      'last-backup': null,
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Stopped,
        },
        'dependency-errors': {
          'btc-rpc-proxy': {
            type: DependencyErrorType.ConfigUnsatisfied,
            error: 'This is a config unsatisfied error',
          },
        },
      },
      'address-info': {
        ui: {
          name: 'Web UI',
          description: 'The browser web interface for LND',
          addresses: [
            'http://lnd-ui-address.onion',
            'https://lnd-ui-address.local',
            'https://192.168.1.1:3449',
          ],
          ui: true,
        },
        grpc: {
          name: 'gRPC',
          description: 'For connecting to LND gRPC interface',
          addresses: [
            'http://lnd-grpc-address.onion',
            'https://lnd-grpc-address.local',
            'https://192.168.1.1:3449',
          ],
          ui: true,
        },
      },
      'current-dependencies': {
        bitcoind: {
          'health-checks': [],
        },
        'btc-rpc-proxy': {
          'health-checks': [],
        },
      },
      'dependency-info': {
        bitcoind: {
          title: 'Bitcoin Core',
          icon: 'assets/img/service-icons/bitcoind.svg',
        },
        'btc-rpc-proxy': {
          title: 'Bitcoin Proxy',
          icon: 'assets/img/service-icons/btc-rpc-proxy.png',
        },
      },
      'marketplace-url': 'https://registry.start9.com/',
      'developer-key': 'developer-key',
      'has-config': true,
    },
    actions: {},
  }

  export const LocalPkgs: { [key: string]: PackageDataEntry } = {
    bitcoind,
    'btc-rpc-proxy': bitcoinProxy,
    lnd,
  }
}
