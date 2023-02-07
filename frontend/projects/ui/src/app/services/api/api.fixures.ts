import {
  DependencyErrorType,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
  ServerStatusInfo,
} from 'src/app/services/patch-db/data-model'
import { Metric, RR, NotificationLevel, ServerNotifications } from './api.types'
import { BTC_ICON, LND_ICON, PROXY_ICON } from './api-icons'
import {
  DependencyMetadata,
  MarketplacePkg,
  Manifest,
} from '@start9labs/marketplace'
import { Log } from '@start9labs/shared'
import { unionSelectKey } from 'start-sdk/lib/config/configTypes'

export module Mock {
  export const ServerUpdated: ServerStatusInfo = {
    'backup-progress': null,
    'update-progress': null,
    updated: true,
    'shutting-down': false,
  }
  export const MarketplaceEos: RR.GetMarketplaceEosRes = {
    version: '0.3.4',
    headline: 'Our biggest release ever.',
    'release-notes': {
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
          how: 'You can use an external node from your Embassy if you prefer.',
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
      message: 'Embassy and services have been successfully backed up.',
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

  export function getServerMetrics() {
    return {
      Group1: {
        Metric1: {
          value: Math.random(),
          unit: 'mi/b',
        },
        Metric2: {
          value: Math.random(),
          unit: '%',
        },
        Metric3: {
          value: 10.1,
          unit: '%',
        },
      },
      Group2: {
        Hmmmm1: {
          value: 22.2,
          unit: 'mi/b',
        },
        Hmmmm2: {
          value: 50,
          unit: '%',
        },
        Hmmmm3: {
          value: 10.1,
          unit: '%',
        },
      },
      Group3: {
        Hmmmm1: {
          value: Math.random(),
          unit: 'mi/b',
        },
        Hmmmm2: {
          value: 50,
          unit: '%',
        },
        Hmmmm3: {
          value: 10.1,
          unit: '%',
        },
      },
      Group4: {
        Hmmmm1: {
          value: Math.random(),
          unit: 'mi/b',
        },
        Hmmmm2: {
          value: 50,
          unit: '%',
        },
        Hmmmm3: {
          value: 10.1,
          unit: '%',
        },
      },
      Group5: {
        Hmmmm1: {
          value: Math.random(),
          unit: 'mi/b',
        },
        Hmmmm2: {
          value: 50,
          unit: '%',
        },
        Hmmmm3: {
          value: 10.1,
          unit: '%',
        },
        Hmmmm4: {
          value: Math.random(),
          unit: 'mi/b',
        },
        Hmmmm5: {
          value: 50,
          unit: '%',
        },
        Hmmmm6: {
          value: 10.1,
          unit: '%',
        },
      },
    }
  }

  export function getAppMetrics() {
    const metr: Metric = {
      Metric1: {
        value: Math.random(),
        unit: 'mi/b',
      },
      Metric2: {
        value: Math.random(),
        unit: '%',
      },
      Metric3: {
        value: 10.1,
        unit: '%',
      },
    }

    return metr
  }

  export const ServerLogs: Log[] = [
    {
      timestamp: '2022-07-28T03:52:54.808769Z',
      message: '****** START *****',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      message:
        '\u001b[34mPOST \u001b[0;32;49m200\u001b[0m photoview.embassy/api/graphql \u001b[0;36;49m1.169406ms\u001b',
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
      '9513226517c54ddd8107d6d7b9d8aed7': {
        'last-active': '2021-07-14T20:49:17.774Z',
        'user-agent': 'AppleWebKit/{WebKit Rev} (KHTML, like Gecko)',
        metadata: {
          platforms: ['iphone', 'mobileweb', 'mobile', 'ios'],
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
    hsbdjhasbasda: {
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
    ftcvewdnkemfksdm: {
      type: 'cloud',
      name: 'Dropbox 1',
      provider: 'dropbox',
      path: '/Home/backups',
      mountable: true,
      'embassy-os': null,
    },
    csgashbdjkasnd: {
      type: 'cifs',
      name: 'Network Folder 2',
      hostname: 'smb://192.169.10.0',
      path: '/Desktop/embassy-backups-2',
      username: 'TestUser',
      mountable: true,
      'embassy-os': null,
    },
    powjefhjbnwhdva: {
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
  }

  export const BackupJobs: RR.GetBackupJobsRes = [
    {
      id: 'lalalalalala-babababababa',
      name: 'My Backup Job',
      target: BackupTargets['hsbdjhasbasda'],
      cron: '0 3 * * *',
      'package-ids': ['bitcoind', 'lnd'],
    },
    {
      id: 'hahahahaha-mwmwmwmwmwmw',
      name: 'Another Backup Job',
      target: BackupTargets['ftcvewdnkemfksdm'],
      cron: '0 * * * *',
      'package-ids': ['lnd'],
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

  export const PackageProperties: RR.GetPackagePropertiesRes<2> = {
    version: 2,
    data: {
      lndconnect: {
        type: 'text',
        inputmode: 'text',
        description: 'This is some information about the thing.',
        copyable: true,
        qr: true,
        masked: true,
        value:
          'lndconnect://udlyfq2mxa4355pt7cqlrdipnvk2tsl4jtsdw7zaeekenufwcev2wlad.onion:10009?cert=MIICJTCCAcugAwIBAgIRAOyq85fqAiA3U3xOnwhH678wCgYIKoZIzj0EAwIwODEfMB0GAkUEChMWbG5kIGF1dG9nZW5lcmF0ZWQgY2VydDEVMBMGA1UEAxMMNTc0OTkwMzIyYzZlMB4XDTIwMTAyNjA3MzEyN1oXDTIxMTIyMTA3MzEyN1owODEfMB0GA1UEChMWbG5kIGF1dG9nZW5lcmF0ZWQgY2VydDEVMBMGA1UEAxMMNTc0OTkwMzIyYzZlMFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEKqfhAMMZdY-eFnU5P4bGrQTSx0lo7m8u4V0yYkzUM6jlql_u31_mU2ovLTj56wnZApkEjoPl6fL2yasZA2wiy6OBtTCBsjAOBgNVHQ8BAf8EBAMCAqQwEwYDVR0lBAwwCgYIKwYBBQUHAwEwDwYDVR0TAQH_BAUwAwEB_zAdBgNVHQ4EFgQUYQ9uIO6spltnVCx4rLFL5BvBF9IwWwYDVR0RBFQwUoIMNTc0OTkwMzIyYzZlgglsb2NhbGhvc3SCBHVuaXiCCnVuaXhwYWNrZXSCB2J1ZmNvbm6HBH8AAAGHEAAAAAAAAAAAAAAAAAAAAAGHBKwSAAswCgYIKoZIzj0EAwIDSAAwRQIgVZH2Z2KlyAVY2Q2aIQl0nsvN-OEN49wreFwiBqlxNj4CIQD5_JbpuBFJuf81I5J0FQPtXY-4RppWOPZBb-y6-rkIUQ&macaroon=AgEDbG5kAusBAwoQuA8OUMeQ8Fr2h-f65OdXdRIBMBoWCgdhZGRyZXNzEgRyZWFkEgV3cml0ZRoTCgRpbmZvEgRyZWFkEgV3cml0ZRoXCghpbnZvaWNlcxIEcmVhZBIFd3JpdGUaFAoIbWFjYXJvb24SCGdlbmVyYXRlGhYKB21lc3NhZ2USBHJlYWQSBXdyaXRlGhcKCG9mZmNoYWluEgRyZWFkEgV3cml0ZRoWCgdvbmNoYWluEgRyZWFkEgV3cml0ZRoUCgVwZWVycxIEcmVhZBIFd3JpdGUaGAoGc2lnbmVyEghnZW5lcmF0ZRIEcmVhZAAABiCYsRUoUWuAHAiCSLbBR7b_qULDSl64R8LIU2aqNIyQfA',
      },
      Nested: {
        type: 'object',
        description: 'This is a nested thing metric',
        value: {
          'Last Name': {
            type: 'text',
            inputmode: 'text',
            description: 'The last name of the user',
            copyable: true,
            qr: true,
            masked: false,
            value: 'Hill',
          },
          Age: {
            type: 'text',
            inputmode: 'text',
            description: 'The age of the user',
            copyable: false,
            qr: false,
            masked: false,
            value: '35',
          },
          Password: {
            type: 'text',
            inputmode: 'text',
            description: 'A secret password',
            copyable: true,
            qr: false,
            masked: true,
            value: 'password123',
          },
        },
      },
      'Another Value': {
        type: 'text',
        inputmode: 'text',
        description: 'Some more information about the service.',
        copyable: false,
        qr: true,
        masked: false,
        value: 'https://guessagain.com',
      },
    },
  } as any // @TODO why is this necessary?

  export const InputSpec: RR.GetPackageConfigRes['spec'] = {
    bitcoin: {
      type: 'object',
      name: 'Bitcoin Settings',
      description:
        'RPC and P2P interface configuration options for Bitcoin Core',
      warning: null,
      spec: {
        'bitcoind-p2p': {
          type: 'union',
          name: 'Bitcoin Core P2P',
          description:
            '<p>The Bitcoin Core node to connect to over the peer-to-peer (P2P) interface:</p><ul><li><strong>Bitcoin Core</strong>: The Bitcoin Core service installed on this device</li><li><strong>External Node</strong>: A Bitcoin node running on a different device</li></ul>',
          warning: null,
          default: null,
          required: true,
          variants: {
            internal: { name: 'Internal', spec: {} },
            external: {
              name: 'External',
              spec: {
                'p2p-host': {
                  type: 'text',
                  inputmode: 'text',
                  name: 'Public Address',
                  description: 'The public address of your Bitcoin Core server',
                  required: true,
                  masked: false,
                  placeholder: null,
                  minLength: 4,
                  maxLength: 20,
                  patterns: [],
                  warning: null,
                  default: null,
                },
                'p2p-port': {
                  type: 'number',
                  name: 'P2P Port',
                  description:
                    'The port that your Bitcoin Core P2P server is bound to',
                  required: true,
                  min: 0,
                  max: 65535,
                  integer: true,
                  step: '1',
                  default: 8333,
                  placeholder: null,
                  warning: null,
                  units: null,
                },
              },
            },
          },
        },
      },
    },
    background: {
      name: 'Background',
      type: 'color',
      description: 'Background color for the service',
      warning: null,
      required: false,
      default: '#000000',
    },
    chronos: {
      name: 'Chronos',
      type: 'object',
      description: 'Various time related settings',
      warning: null,
      spec: {
        time: {
          name: 'Time',
          type: 'datetime',
          inputmode: 'time',
          description: 'Time of day',
          warning: null,
          required: true,
          min: '12:00',
          max: '16:00',
          step: null,
          default: '12:00',
        },
        date: {
          name: 'Date',
          type: 'datetime',
          inputmode: 'date',
          description: 'Just a date',
          warning: null,
          required: true,
          min: '2023-01-01',
          max: '2023-12-31',
          step: null,
          default: '2023-05-01',
        },
        datetime: {
          name: 'Date and time',
          type: 'datetime',
          inputmode: 'datetime-local',
          description: 'Both date and time',
          warning: null,
          required: true,
          min: '2023-01-01T12:00',
          max: '2023-12-31T16:00',
          step: null,
          default: '2023-05-01T18:30',
        },
      },
    },
    advanced: {
      name: 'Advanced',
      type: 'object',
      description: 'Advanced settings',
      warning: null,
      spec: {
        rpcsettings: {
          name: 'RPC Settings',
          type: 'object',
          description: 'rpc username and password',
          warning:
            'Adding RPC users gives them special permissions on your node.',
          spec: {
            rpcuser2: {
              name: 'RPC Username',
              type: 'text',
              inputmode: 'text',
              description: 'rpc username',
              warning: null,
              placeholder: null,
              minLength: null,
              maxLength: null,
              required: true,
              default: 'defaultrpcusername',
              patterns: [
                {
                  regex: '^[a-zA-Z]+$',
                  description: 'must contain only letters.',
                },
              ],
              masked: false,
            },
            rpcuser: {
              name: 'RPC Username',
              type: 'text',
              inputmode: 'text',
              description: 'rpc username',
              warning: null,
              placeholder: null,
              required: true,
              minLength: null,
              maxLength: null,
              default: 'defaultrpcusername',
              patterns: [
                {
                  regex: '^[a-zA-Z]+$',
                  description: 'must contain only letters.',
                },
              ],
              masked: false,
            },
            rpcpass: {
              name: 'RPC User Password',
              type: 'text',
              inputmode: 'text',
              description: 'rpc password',
              placeholder: null,
              minLength: null,
              maxLength: null,
              warning: null,
              required: true,
              default: {
                charset: 'a-z,A-Z,2-9',
                len: 20,
              },
              masked: true,
              patterns: [],
            },
            rpcpass2: {
              name: 'RPC User Password',
              type: 'text',
              inputmode: 'text',
              description: 'rpc password',
              warning: null,
              placeholder: null,
              minLength: null,
              maxLength: null,
              required: true,
              default: {
                charset: 'a-z,A-Z,2-9',
                len: 20,
              },
              masked: true,
              patterns: [],
            },
          },
        },
      },
    },
    bio: {
      name: 'Bio',
      type: 'textarea',
      description: 'Your personal bio',
      placeholder: 'Tell the world about yourself',
      minLength: null,
      maxLength: null,
      warning: null,
      required: false,
    },
    testnet: {
      name: 'Testnet',
      type: 'toggle',
      description:
        '<ul><li>determines whether your node is running on testnet or mainnet</li></ul><script src="fake"></script>',
      warning: 'Chain will have to resync!',
      default: true,
    },
    document: {
      name: 'Needed File',
      type: 'file',
      description: 'A file we need',
      warning: 'Testing warning',
      required: true,
      extensions: ['.png'],
    },
    'object-list': {
      name: 'Object List',
      type: 'list',
      description: 'This is a list of objects, like users or something',
      warning: null,
      minLength: null,
      maxLength: 4,
      default: [
        {
          'first-name': 'Admin',
          'last-name': 'User',
          age: 40,
        },
        {
          'first-name': 'Admin2',
          'last-name': 'User',
          age: 40,
        },
      ],
      // the outer spec here, at the list level, says that what's inside (the inner spec) pertains to its inner elements.
      // it just so happens that ValueSpecObject's have the field { spec: InputSpec }
      spec: {
        type: 'object',
        uniqueBy: 'last-name',
        displayAs: `I'm {{last-name}}, {{first-name}} {{last-name}}`,
        spec: {
          'first-name': {
            name: 'First Name',
            type: 'text',
            inputmode: 'text',
            description: 'User first name',
            required: false,
            masked: false,
            minLength: 4,
            maxLength: 15,
            placeholder: null,
            patterns: [],
            warning: null,
            default: null,
          },
          'last-name': {
            name: 'Last Name',
            type: 'text',
            inputmode: 'text',
            description: 'User first name',
            minLength: null,
            maxLength: null,
            required: false,
            default: {
              charset: 'a-g,2-9',
              len: 12,
            },
            patterns: [
              {
                regex: '^[a-zA-Z]+$',
                description: 'must contain only letters.',
              },
            ],
            masked: false,
            placeholder: null,
            warning: null,
          },
          age: {
            name: 'Age',
            type: 'number',
            description: 'The age of the user',
            required: false,
            integer: false,
            warning: 'User must be at least 18.',
            min: 18,
            max: null,
            step: null,
            units: null,
            placeholder: null,
            default: null,
          },
        },
      },
    },
    'random-select': {
      name: 'Random Select',
      type: 'select',
      values: {
        hello: 'Hello',
        goodbye: 'Goodbye',
        sup: 'Sup',
      },
      default: 'sup',
      description: 'This is not even real.',
      warning: 'Be careful changing this!',
      required: true,
    },
    notifications: {
      name: 'Notification Preferences',
      type: 'multiselect',
      description: 'how you want to be notified',
      warning: null,
      minLength: 2,
      maxLength: 3,
      values: {
        email: 'EEEEmail',
        text: 'Texxxt',
        call: 'Ccccall',
        push: 'PuuuusH',
        webhook: 'WebHooookkeee',
      },
      default: ['email'],
    },
    'favorite-number': {
      name: 'Favorite Number',
      type: 'number',
      integer: false,
      description: 'Your favorite number of all time',
      placeholder: null,
      warning:
        'Once you set this number, it can never be changed without severe consequences.',
      required: false,
      default: 7,
      min: -99,
      max: 100,
      step: 'all',
      units: 'BTC',
    },
    'unlucky-numbers': {
      name: 'Unlucky Numbers',
      type: 'list',
      description: 'Numbers that you like but are not your top favorite.',
      warning: null,
      spec: {
        type: 'number',
        integer: false,
        min: -10,
        max: 10,
        step: null,
        units: null,
        placeholder: null,
      },
      minLength: null,
      maxLength: 10,
      default: [2, 3],
    },
    rpcsettings: {
      name: 'RPC Settings',
      type: 'object',
      description: 'rpc username and password',
      warning: 'Adding RPC users gives them special permissions on your node.',
      spec: {
        laws: {
          name: 'Laws',
          type: 'object',
          description: 'the law of the realm',
          warning: null,
          spec: {
            law1: {
              name: 'First Law',
              type: 'text',
              inputmode: 'text',
              description: 'the first law',
              required: false,
              masked: false,
              minLength: null,
              maxLength: null,
              placeholder: null,
              patterns: [],
              warning: null,
              default: null,
            },
            law2: {
              name: 'Second Law',
              type: 'text',
              inputmode: 'text',
              description: 'the second law',
              required: false,
              masked: false,
              minLength: null,
              maxLength: null,
              placeholder: null,
              patterns: [],
              warning: null,
              default: null,
            },
          },
        },
        rulemakers: {
          name: 'Rule Makers',
          type: 'list',
          description: 'the people who make the rules',
          warning: null,
          minLength: 1,
          maxLength: 3,
          default: [],
          spec: {
            type: 'object',
            uniqueBy: null,
            displayAs: null,
            spec: {
              rulemakername: {
                name: 'Rulemaker Name',
                type: 'text',
                inputmode: 'text',
                description: 'the name of the rule maker',
                required: true,
                minLength: null,
                maxLength: 30,
                default: {
                  charset: 'a-g,2-9',
                  len: 12,
                },
                masked: false,
                placeholder: null,
                patterns: [],
                warning: null,
              },
              rulemakerip: {
                name: 'Rulemaker IP',
                type: 'text',
                inputmode: 'text',
                description: 'the ip of the rule maker',
                required: true,
                default: '192.168.1.0',
                minLength: 4,
                maxLength: 20,
                patterns: [
                  {
                    regex:
                      '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                    description: 'may only contain numbers and periods',
                  },
                ],
                masked: false,
                placeholder: null,
                warning: null,
              },
            },
          },
        },
        rpcuser: {
          name: 'RPC Username',
          type: 'text',
          inputmode: 'text',
          description: 'rpc username',
          required: true,
          minLength: null,
          maxLength: null,
          default: 'defaultrpcusername',
          patterns: [
            {
              regex: '^[a-zA-Z]+$',
              description: 'must contain only letters.',
            },
          ],
          masked: false,
          placeholder: null,
          warning: null,
        },
        rpcpass: {
          name: 'RPC User Password',
          type: 'text',
          inputmode: 'text',
          description: 'rpc password',
          minLength: null,
          maxLength: null,
          required: true,
          default: {
            charset: 'a-z,A-Z,2-9',
            len: 20,
          },
          masked: true,
          placeholder: null,
          patterns: [],
          warning: null,
        },
      },
    },
    'bitcoin-node': {
      type: 'union',
      default: 'internal',
      name: 'Bitcoin Node Settings',
      description: 'Options<ul><li>Item 1</li><li>Item 2</li></ul>',
      warning: 'Careful changing this',
      required: true,
      variants: {
        dummy: {
          name: 'Dummy',
          spec: {
            name: {
              type: 'text',
              inputmode: 'text',
              name: 'Name',
              description: null,
              minLength: null,
              maxLength: null,
              required: true,
              masked: false,
              patterns: [
                {
                  regex: '^[a-zA-Z]+$',
                  description: 'must contain only letters.',
                },
              ],
              placeholder: null,
              warning: null,
              default: null,
            },
          },
        },
        internal: { name: 'Internal', spec: {} },
        external: {
          name: 'External',
          spec: {
            'emergency-contact': {
              name: 'Emergency Contact',
              type: 'object',
              description: 'The person to contact in case of emergency.',
              warning: null,
              spec: {
                name: {
                  type: 'text',
                  inputmode: 'text',
                  name: 'Name',
                  description: null,
                  required: true,
                  minLength: null,
                  maxLength: null,
                  masked: false,
                  patterns: [
                    {
                      regex: '^[a-zA-Z]+$',
                      description: 'must contain only letters.',
                    },
                  ],
                  placeholder: null,
                  warning: null,
                  default: null,
                },
                email: {
                  type: 'text',
                  inputmode: 'text',
                  name: 'Email',
                  description: null,
                  required: true,
                  minLength: null,
                  maxLength: null,
                  masked: false,
                  placeholder: null,
                  patterns: [],
                  warning: null,
                  default: null,
                },
              },
            },
            'public-domain': {
              name: 'Public Domain',
              type: 'text',
              inputmode: 'text',
              description: 'the public address of the node',
              required: true,
              default: 'bitcoinnode.com',
              minLength: null,
              maxLength: null,
              patterns: [],
              masked: false,
              placeholder: null,
              warning: null,
            },
            'private-domain': {
              name: 'Private Domain',
              type: 'text',
              inputmode: 'text',
              description: 'the private address of the node',
              required: true,
              minLength: null,
              maxLength: null,
              masked: true,
              placeholder: null,
              patterns: [],
              warning: null,
              default: null,
            },
          },
        },
      },
    },
    port: {
      name: 'Port',
      type: 'number',
      integer: true,
      description:
        'the default port for your Bitcoin node. default: 8333, testnet: 18333, regtest: 18444',
      warning: null,
      required: true,
      min: 1,
      max: 9999,
      step: '1',
      units: null,
      placeholder: null,
      default: null,
    },
    'favorite-slogan': {
      name: 'Favorite Slogan',
      type: 'text',
      inputmode: 'text',
      description:
        'You most favorite slogan in the whole world, used for paying you.',
      required: false,
      masked: true,
      minLength: null,
      maxLength: null,
      placeholder: null,
      patterns: [],
      warning: null,
      default: null,
    },
    rpcallowip: {
      name: 'RPC Allowed IPs',
      type: 'list',
      description:
        'external ip addresses that are authorized to access your Bitcoin node',
      warning:
        'Any IP you allow here will have RPC access to your Bitcoin node.',
      minLength: 1,
      maxLength: 10,
      default: ['192.168.1.1'],
      spec: {
        type: 'text',
        inputmode: 'text',
        masked: false,
        placeholder: null,
        minLength: 4,
        maxLength: 20,
        patterns: [
          {
            regex:
              '((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))',
            description: 'must be a valid ipv4, ipv6, or domain name',
          },
        ],
      },
    },
    rpcauth: {
      name: 'RPC Auth',
      type: 'list',
      description: 'api keys that are authorized to access your Bitcoin node.',
      warning: null,
      minLength: null,
      maxLength: null,
      default: [],
      spec: {
        type: 'text',
        inputmode: 'text',
        masked: false,
        minLength: null,
        maxLength: null,
        placeholder: null,
        patterns: [],
      },
    },
    'more-advanced': {
      name: 'More Advanced',
      type: 'object',
      description: 'Advanced settings',
      warning: null,
      spec: {
        rpcsettings: {
          name: 'RPC Settings',
          type: 'object',
          description: 'rpc username and password',
          warning:
            'Adding RPC users gives them special permissions on your node.',
          spec: {
            laws: {
              name: 'Laws',
              type: 'object',
              description: 'the law of the realm',
              warning: null,
              spec: {
                law1: {
                  name: 'First Law',
                  type: 'text',
                  inputmode: 'text',
                  description: 'the first law',
                  required: false,
                  masked: false,
                  placeholder: null,
                  patterns: [],
                  minLength: null,
                  maxLength: null,
                  warning: null,
                  default: null,
                },
                law2: {
                  name: 'Second Law',
                  type: 'text',
                  inputmode: 'text',
                  description: 'the second law',
                  required: false,
                  masked: false,
                  placeholder: null,
                  patterns: [],
                  minLength: null,
                  maxLength: null,
                  warning: null,
                  default: null,
                },
                law4: {
                  name: 'Fourth Law',
                  type: 'text',
                  inputmode: 'text',
                  description: 'the fourth law',
                  required: false,
                  masked: false,
                  placeholder: null,
                  patterns: [],
                  minLength: null,
                  maxLength: null,
                  warning: null,
                  default: null,
                },
                law3: {
                  name: 'Third Law',
                  type: 'list',
                  description: 'the third law',
                  warning: null,
                  minLength: null,
                  maxLength: 2,
                  default: [],
                  spec: {
                    type: 'object',
                    uniqueBy: null,
                    displayAs: null,
                    spec: {
                      lawname: {
                        name: 'Law Name',
                        type: 'text',
                        inputmode: 'text',
                        description: 'the name of the law maker',
                        required: true,
                        default: {
                          charset: 'a-g,2-9',
                          len: 12,
                        },
                        masked: false,
                        placeholder: null,
                        patterns: [],
                        minLength: null,
                        maxLength: null,
                        warning: null,
                      },
                      lawagency: {
                        name: 'Law agency',
                        type: 'text',
                        inputmode: 'text',
                        description: 'the ip of the law maker',
                        required: true,
                        default: '192.168.1.0',
                        minLength: null,
                        maxLength: null,
                        patterns: [
                          {
                            regex:
                              '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                            description: 'may only contain numbers and periods',
                          },
                        ],
                        masked: false,
                        placeholder: null,
                        warning: null,
                      },
                    },
                  },
                },
                law5: {
                  name: 'Fifth Law',
                  type: 'text',
                  inputmode: 'text',
                  description: 'the fifth law',
                  required: false,
                  masked: false,
                  placeholder: null,
                  minLength: null,
                  maxLength: null,
                  patterns: [],
                  warning: null,
                  default: null,
                },
              },
            },
            rulemakers: {
              name: 'Rule Makers',
              type: 'list',
              description: 'the people who make the rules',
              warning: null,
              minLength: null,
              maxLength: 2,
              default: [],
              spec: {
                type: 'object',
                uniqueBy: null,
                displayAs: null,
                spec: {
                  rulemakername: {
                    name: 'Rulemaker Name',
                    type: 'text',
                    inputmode: 'text',
                    description: 'the name of the rule maker',
                    required: true,
                    default: {
                      charset: 'a-g,2-9',
                      len: 12,
                    },
                    masked: false,
                    placeholder: null,
                    patterns: [],
                    minLength: null,
                    maxLength: null,
                    warning: null,
                  },
                  rulemakerip: {
                    name: 'Rulemaker IP',
                    type: 'text',
                    inputmode: 'text',
                    description: 'the ip of the rule maker',
                    required: true,
                    default: '192.168.1.0',
                    minLength: null,
                    maxLength: null,
                    patterns: [
                      {
                        regex:
                          '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                        description: 'may only contain numbers and periods',
                      },
                    ],
                    masked: false,
                    placeholder: null,
                    warning: null,
                  },
                },
              },
            },
            rpcuser: {
              name: 'RPC Username',
              type: 'text',
              inputmode: 'text',
              description: 'rpc username',
              required: true,
              default: 'defaultrpcusername',
              minLength: null,
              maxLength: null,
              patterns: [
                {
                  regex: '^[a-zA-Z]+$',
                  description: 'must contain only letters.',
                },
              ],
              masked: false,
              placeholder: null,
              warning: null,
            },
            rpcpass: {
              name: 'RPC User Password',
              type: 'text',
              inputmode: 'text',
              description: 'rpc password',
              required: true,
              default: {
                charset: 'a-z,A-Z,2-9',
                len: 20,
              },
              masked: true,
              placeholder: null,
              patterns: [],
              minLength: null,
              maxLength: null,
              warning: null,
            },
          },
        },
      },
    },
  }

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
    manifest: MockManifestBitcoind,
    icon: '/assets/img/service-icons/bitcoind.png',
    installed: {
      'last-backup': null,
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Running,
          started: new Date().toISOString(),
          health: {},
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
          icon: 'assets/img/service-icons/bitcoind.png',
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
            type: DependencyErrorType.NotInstalled,
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
          icon: 'assets/img/service-icons/bitcoind.png',
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
