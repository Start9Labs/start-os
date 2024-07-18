import {
  InstalledState,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { Metric, NotificationLevel, RR, ServerNotifications } from './api.types'
import { BTC_ICON, PROXY_ICON, REGISTRY_ICON } from './api-icons'
import { Log } from '@start9labs/shared'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { T, CB } from '@start9labs/start-sdk'

const mockBlake3Commitment: T.Blake3Commitment = {
  hash: 'fakehash',
  size: 0,
}

const mockMerkleArchiveCommitment: T.MerkleArchiveCommitment = {
  rootSighash: 'fakehash',
  rootMaxsize: 0,
}

const mockDescription = {
  short: 'Lorem ipsum dolor sit amet',
  long: 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
}

export module Mock {
  export const ServerUpdated: T.ServerStatus = {
    backupProgress: null,
    updateProgress: null,
    updated: true,
    restarting: false,
    shuttingDown: false,
  }
  export const MarketplaceEos: RR.CheckOSUpdateRes = {
    version: '0.3.6',
    headline: 'Our biggest release ever.',
    releaseNotes: {
      '0.3.6': 'Some **Markdown** release _notes_ for 0.3.6',
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

  export const RegistryInfo: RR.GetRegistryInfoRes = {
    name: 'Start9 Registry',
    icon: REGISTRY_ICON,
    categories: {
      bitcoin: {
        name: 'Bitcoin',
        description: mockDescription,
      },
      // TODO: add more
    },
  }

  export const MockManifestBitcoind: T.Manifest = {
    id: 'bitcoind',
    title: 'Bitcoin Core',
    version: '0.21.0',
    satisfies: [],
    gitHash: 'abcdefgh',
    description: {
      short: 'A Bitcoin full node by Bitcoin Core.',
      long: 'Bitcoin is a decentralized consensus protocol and settlement network.',
    },
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
    images: {
      main: {
        source: 'packed',
        arch: ['x86_64', 'aarch64'],
        emulateMissingAs: 'aarch64',
      },
    },
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
    satisfies: [],
    gitHash: 'abcdefgh',
    description: {
      short: 'A bolt spec compliant client.',
      long: 'More info about LND. More info about LND. More info about LND.',
    },
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
        s9pk: '',
      },
      'btc-rpc-proxy': {
        description:
          'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
        optional: true,
        s9pk: '',
      },
    },
    hasConfig: true,
    images: {
      main: {
        source: 'packed',
        arch: ['x86_64', 'aarch64'],
        emulateMissingAs: 'aarch64',
      },
    },
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
    satisfies: [],
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
        s9pk: '',
      },
    },
    hasConfig: false,
    images: {
      main: {
        source: 'packed',
        arch: ['x86_64', 'aarch64'],
        emulateMissingAs: 'aarch64',
      },
    },
    assets: [],
    volumes: ['main'],
    hardwareRequirements: {
      device: {},
      arch: null,
      ram: null,
    },
  }

  export const BitcoinDep: T.DependencyMetadata = {
    title: 'Bitcoin Core',
    icon: BTC_ICON,
    optional: false,
    description: 'Needed to run',
  }

  export const ProxyDep: T.DependencyMetadata = {
    title: 'Bitcoin Proxy',
    icon: PROXY_ICON,
    optional: true,
    description: 'Needed to run',
  }

  export const RegistryPackages: {
    [id: T.PackageId]: T.GetPackageResponseFull
  } = {
    bitcoind: {
      best: {
        '27.0.0': {
          title: 'Bitcoin Core',
          description: mockDescription,
          hardwareRequirements: { arch: null, device: {}, ram: null },
          license: 'mit',
          wrapperRepo: 'https://github.com/start9labs/bitcoind-startos',
          upstreamRepo: 'https://github.com/bitcoin/bitcoin',
          supportSite: 'https://bitcoin.org',
          marketingSite: 'https://bitcoin.org',
          releaseNotes: 'Even better support for Bitcoin and wallets!',
          osVersion: '0.3.6',
          gitHash: 'fakehash',
          icon: BTC_ICON,
          sourceVersion: null,
          dependencyMetadata: {},
          donationUrl: null,
          alerts: {
            install: 'test',
            uninstall: 'test',
            start: 'test',
            stop: 'test',
            restore: 'test',
          },
          s9pk: {
            url: 'https://github.com/Start9Labs/bitcoind-startos/releases/download/v27.0.0/bitcoind.s9pk',
            commitment: mockMerkleArchiveCommitment,
            signatures: {},
            publishedAt: Date.now().toString(),
          },
        },
        'knots-26.1.20240513': {
          title: 'Bitcoin Knots',
          description: mockDescription,
          hardwareRequirements: { arch: null, device: {}, ram: null },
          license: 'mit',
          wrapperRepo: 'https://github.com/start9labs/bitcoinknots-startos',
          upstreamRepo: 'https://github.com/bitcoinknots/bitcoin',
          supportSite: 'https://bitcoinknots.org',
          marketingSite: 'https://bitcoinknots.org',
          releaseNotes: 'Even better support for Bitcoin and wallets!',
          osVersion: '0.3.6',
          gitHash: 'fakehash',
          icon: BTC_ICON,
          sourceVersion: null,
          dependencyMetadata: {},
          donationUrl: null,
          alerts: {
            install: 'test',
            uninstall: 'test',
            start: 'test',
            stop: 'test',
            restore: 'test',
          },
          s9pk: {
            url: 'https://github.com/Start9Labs/bitcoinknots-startos/releases/download/v26.1.20240513/bitcoind.s9pk',
            commitment: mockMerkleArchiveCommitment,
            signatures: {},
            publishedAt: Date.now().toString(),
          },
        },
      },
      categories: ['bitcoin'],
      otherVersions: {
        '26.1.0': {
          title: 'Bitcoin Core',
          description: mockDescription,
          hardwareRequirements: { arch: null, device: {}, ram: null },
          license: 'mit',
          wrapperRepo: 'https://github.com/start9labs/bitcoind-startos',
          upstreamRepo: 'https://github.com/bitcoin/bitcoin',
          supportSite: 'https://bitcoin.org',
          marketingSite: 'https://bitcoin.org',
          releaseNotes: 'Even better support for Bitcoin and wallets!',
          osVersion: '0.3.6',
          gitHash: 'fakehash',
          icon: BTC_ICON,
          sourceVersion: null,
          dependencyMetadata: {},
          donationUrl: null,
          alerts: {
            install: 'test',
            uninstall: 'test',
            start: 'test',
            stop: 'test',
            restore: 'test',
          },
          s9pk: {
            url: 'https://github.com/Start9Labs/bitcoind-startos/releases/download/v26.1.0/bitcoind.s9pk',
            commitment: mockMerkleArchiveCommitment,
            signatures: {},
            publishedAt: Date.now().toString(),
          },
        },
        'knots-26.1.20240325': {
          title: 'Bitcoin Knots',
          description: mockDescription,
          hardwareRequirements: { arch: null, device: {}, ram: null },
          license: 'mit',
          wrapperRepo: 'https://github.com/start9labs/bitcoinknots-startos',
          upstreamRepo: 'https://github.com/bitcoinknots/bitcoin',
          supportSite: 'https://bitcoinknots.org',
          marketingSite: 'https://bitcoinknots.org',
          releaseNotes: 'Even better support for Bitcoin and wallets!',
          osVersion: '0.3.6',
          gitHash: 'fakehash',
          icon: BTC_ICON,
          sourceVersion: null,
          dependencyMetadata: {},
          donationUrl: null,
          alerts: {
            install: 'test',
            uninstall: 'test',
            start: 'test',
            stop: 'test',
            restore: 'test',
          },
          s9pk: {
            url: 'https://github.com/Start9Labs/bitcoinknots-startos/releases/download/v26.1.20240325/bitcoind.s9pk',
            commitment: mockMerkleArchiveCommitment,
            signatures: {},
            publishedAt: Date.now().toString(),
          },
        },
      },
    },
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
    },
    {
      id: 4,
      packageId: 'bitcoind',
      createdAt: '2019-12-26T14:20:30.872Z',
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
      bootId: 'hsjnfdklasndhjasvbjamsksajbndjn',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      message:
        '\u001b[34mPOST \u001b[0;32;49m200\u001b[0m photoview.startos/api/graphql \u001b[0;36;49m1.169406ms\u001b',
      bootId: 'hsjnfdklasndhjasvbjamsksajbndjn',
    },
    {
      timestamp: '2019-12-26T14:22:30.872Z',
      message: '****** FINISH *****',
      bootId: 'gvbwfiuasokdasjndasnjdmfvbahjdmdkfm',
    },
    {
      timestamp: '2019-12-26T15:22:30.872Z',
      message: '****** AGAIN *****',
      bootId: 'gvbwfiuasokdasjndasnjdmfvbahjdmdkfm',
    },
  ]

  export const Sessions: RR.GetSessionsRes = {
    current: 'b7b1a9cef4284f00af9e9dda6e676177',
    sessions: {
      '9513226517c54ddd8107d6d7b9d8aed7': {
        lastActive: '2021-07-14T20:49:17.774Z',
        userAgent: 'AppleWebKit/{WebKit Rev} (KHTML, like Gecko)',
        metadata: {
          platforms: ['iphone', 'mobileweb', 'mobile', 'ios'],
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
    hsbdjhasbasda: {
      type: 'cifs',
      hostname: 'smb://192.169.10.0',
      path: '/Desktop/startos-backups',
      username: 'TestUser',
      mountable: false,
      startOs: {
        '1234-5678-9876-5432': {
          hostname: 'adjective-noun',
          timestamp: new Date().toISOString(),
          version: '0.3.6',
          passwordHash:
            // password is asdfasdf
            '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
          wrappedKey: '',
        },
      },
    },
    // 'ftcvewdnkemfksdm': {
    //   type: 'disk',
    //   logicalname: 'sdba1',
    //   label: 'Matt Stuff',
    //   capacity: 1000000000000,
    //   used: 0,
    //   model: 'Evo SATA 2.5',
    //   vendor: 'Samsung',
    //   startOs: {},
    // },
    csgashbdjkasnd: {
      type: 'cifs',
      hostname: 'smb://192.169.10.0',
      path: '/Desktop/startos-backups-2',
      username: 'TestUser',
      mountable: true,
      startOs: {},
    },
    powjefhjbnwhdva: {
      type: 'disk',
      logicalname: 'sdba1',
      label: 'Another Drive',
      capacity: 2000000000000,
      used: 100000000000,
      model: null,
      vendor: 'SSK',
      startOs: {
        '1234-5678-9876-5432': {
          hostname: 'adjective-noun',
          timestamp: new Date().toISOString(),
          version: '0.3.6',
          passwordHash:
            // password is asdfasdf
            '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
          wrappedKey: '',
        },
      },
    },
  }

  export const BackupInfo: RR.GetBackupInfoRes = {
    version: '0.3.6',
    timestamp: new Date().toISOString(),
    packageBackups: {
      bitcoind: {
        title: 'Bitcoin Core',
        version: '0.21.0',
        osVersion: '0.3.6',
        timestamp: new Date().toISOString(),
      },
      'btc-rpc-proxy': {
        title: 'Bitcoin Proxy',
        version: '0.2.2',
        osVersion: '0.3.6',
        timestamp: new Date().toISOString(),
      },
    },
  }

  export const PackageProperties: RR.GetPackagePropertiesRes<2> = {
    version: 2,
    data: {
      lndconnect: {
        type: 'string',
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
            type: 'string',
            description: 'The last name of the user',
            copyable: true,
            qr: true,
            masked: false,
            value: 'Hill',
          },
          Age: {
            type: 'string',
            description: 'The age of the user',
            copyable: false,
            qr: false,
            masked: false,
            value: '35',
          },
          Password: {
            type: 'string',
            description: 'A secret password',
            copyable: true,
            qr: false,
            masked: true,
            value: 'password123',
          },
        },
      },
      'Another Value': {
        type: 'string',
        description: 'Some more information about the service.',
        copyable: false,
        qr: true,
        masked: false,
        value: 'https://guessagain.com',
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
        color: CB.Value.color({
          name: 'Color',
          required: false,
        }),
        datetime: CB.Value.datetime({
          name: 'Datetime',
          required: false,
        }),
        file: CB.Value.file({
          name: 'File',
          required: false,
          extensions: ['png', 'pdf'],
        }),
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
      selection: 'internal',
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
    lastBackup: null,
    status: {
      configured: true,
      main: {
        status: 'running',
        started: new Date().toISOString(),
        health: {},
      },
    },
    actions: {},
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
          internalPort: 80,
          scheme: 'http',
          sslScheme: 'https',
          suffix: '',
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
          internalPort: 8332,
          scheme: 'http',
          sslScheme: 'https',
          suffix: '',
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
          internalPort: 8333,
          scheme: 'bitcoin',
          sslScheme: null,
          suffix: '',
        },
      },
    },
    currentDependencies: {},
    hosts: {
      abcdefg: {
        kind: 'multi',
        bindings: [],
        addresses: [],
        hostnameInfo: {
          80: [
            {
              kind: 'ip',
              networkInterfaceId: 'eth0',
              public: false,
              hostname: {
                kind: 'local',
                value: 'adjective-noun.local',
                port: null,
                sslPort: 1234,
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
                sslPort: 1234,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'eth0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '192.168.10.11',
                port: null,
                sslPort: 1234,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'wlan0',
              public: false,
              hostname: {
                kind: 'ipv4',
                value: '10.0.0.2',
                port: null,
                sslPort: 1234,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'eth0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[FE80:CD00:0000:0CDE:1257:0000:211E:729CD]',
                port: null,
                sslPort: 1234,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'wlan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: '[FE80:CD00:0000:0CDE:1257:0000:211E:1234]',
                port: null,
                sslPort: 1234,
              },
            },
            {
              kind: 'onion',
              hostname: {
                value: 'bitcoin-p2p.onion',
                port: 80,
                sslPort: 443,
              },
            },
          ],
        },
      },
      bcdefgh: {
        kind: 'multi',
        bindings: [],
        addresses: [],
        hostnameInfo: {
          8332: [],
        },
      },
      cdefghi: {
        kind: 'multi',
        bindings: [],
        addresses: [],
        hostnameInfo: {
          8333: [],
        },
      },
    },
    storeExposedDependents: [],
    registry: 'https://registry.start9.com/',
    developerKey: 'developer-key',
  }

  export const bitcoinProxy: PackageDataEntry<InstalledState> = {
    stateInfo: {
      state: 'installed',
      manifest: MockManifestBitcoinProxy,
    },
    icon: '/assets/img/service-icons/btc-rpc-proxy.png',
    lastBackup: null,
    status: {
      configured: false,
      main: {
        status: 'stopped',
      },
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
          internalPort: 80,
          scheme: 'http',
          sslScheme: 'https',
          suffix: '',
        },
      },
    },
    currentDependencies: {
      bitcoind: {
        title: Mock.MockManifestBitcoind.title,
        icon: 'assets/img/service-icons/bitcoind.svg',
        kind: 'running',
        versionRange: '>=26.0.0',
        healthChecks: [],
        configSatisfied: true,
      },
    },
    hosts: {},
    storeExposedDependents: [],
    registry: 'https://registry.start9.com/',
    developerKey: 'developer-key',
  }

  export const lnd: PackageDataEntry<InstalledState> = {
    stateInfo: {
      state: 'installed',
      manifest: MockManifestLnd,
    },
    icon: '/assets/img/service-icons/lnd.png',
    lastBackup: null,
    status: {
      configured: true,
      main: {
        status: 'stopped',
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
          internalPort: 10009,
          scheme: null,
          sslScheme: 'grpc',
          suffix: '',
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
          internalPort: 10009,
          scheme: null,
          sslScheme: 'lndconnect',
          suffix: 'cert=askjdfbjadnaskjnd&macaroon=ksjbdfnhjasbndjksand',
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
          internalPort: 9735,
          scheme: 'lightning',
          sslScheme: null,
          suffix: '',
        },
      },
    },
    currentDependencies: {
      bitcoind: {
        title: Mock.MockManifestBitcoind.title,
        icon: 'assets/img/service-icons/bitcoind.svg',
        kind: 'running',
        versionRange: '>=26.0.0',
        healthChecks: [],
        configSatisfied: true,
      },
      'btc-rpc-proxy': {
        title: Mock.MockManifestBitcoinProxy.title,
        icon: 'assets/img/service-icons/btc-rpc-proxy.png',
        kind: 'exists',
        versionRange: '>2.0.0',
        configSatisfied: false,
      },
    },
    hosts: {},
    storeExposedDependents: [],
    registry: 'https://registry.start9.com/',
    developerKey: 'developer-key',
  }

  export const LocalPkgs: { [key: string]: PackageDataEntry<InstalledState> } =
    {
      bitcoind: bitcoind,
      'btc-rpc-proxy': bitcoinProxy,
      lnd: lnd,
    }
}
