import { DependencyErrorType, DockerIoFormat, Manifest, PackageDataEntry, PackageMainStatus, PackageState } from 'src/app/services/patch-db/data-model'
import { Log, MarketplacePkg, Metric, NotificationLevel, RR, ServerNotifications } from './api.types'

export module Mock {

  export const MarketplaceEos: RR.GetMarketplaceEOSRes = {
    version: '0.3.1',
    headline: 'Our biggest release ever.',
    'release-notes': {
      '0.3.1': 'Some **Markdown** release _notes_ for 0.3.1',
      '0.3.0': 'Some **Markdown** release _notes_ from a prior version',
    },
  }

  export const ReleaseNotes: RR.GetReleaseNotesRes = {
    '0.19.2': 'Contrary to popular belief, Lorem Ipsum is not simply random text.',
    '0.19.1': 'release notes for Bitcoin 0.19.1',
    '0.19.0': 'release notes for Bitcoin 0.19.0',
  }

  export const MockManifestBitcoind: Manifest = {
    'id': 'bitcoind',
    'title': 'Bitcoin Core',
    'version': '0.20.0',
    'description': {
        'short': 'A Bitcoin full node by Bitcoin Core.',
        'long': 'Bitcoin is a decentralized consensus protocol and settlement network.',
    },
    'release-notes': 'Taproot, Schnorr, and more.',
    'license': 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/bitcoind-wrapper',
    'upstream-repo': 'https://github.com/bitcoin/bitcoin',
    'support-site': 'https://bitcoin.org',
    'marketing-site': 'https://bitcoin.org',
    'donation-url': 'https://start9.com',
    'alerts': {
        'install': 'Bitcoin can take over a week to sync.',
        'uninstall': 'Chain state will be lost, as will any funds stored on your Bitcoin Core waller that have not been backed up.',
        'restore': null,
        'start': null,
        'stop': 'Stopping Bitcoin is bad for your health.',
    },
    'main': {
        'type': 'docker',
        'image': '',
        'system': true,
        'entrypoint': '',
        'args': [],
        'mounts': { },
        'io-format': DockerIoFormat.Yaml,
        'inject': false,
        'shm-size': '',
    },
    'health-checks': { },
    'config': {
      'get': null,
      'set': null,
    },
    'volumes': { },
    'min-os-version': '0.2.12',
    'interfaces': {
        'ui': {
            'name': 'Node Visualizer',
            'description': 'Web application for viewing information about your node and the Bitcoin network.',
            'ui': true,
            'tor-config': {
                'port-mapping': { },
            },
            'lan-config': { },
            'protocols': [],
        },
        'rpc': {
            'name': 'RPC',
            'description': 'Used by wallets to interact with your Bitcoin Core node.',
            'ui': false,
            'tor-config': {
                'port-mapping': { },
            },
            'lan-config': { },
            'protocols': [],
        },
        'p2p': {
            'name': 'P2P',
            'description': 'Used by other Bitcoin nodes to communicate and interact with your node.',
            'ui': false,
            'tor-config': {
                'port-mapping': { },
            },
            'lan-config': { },
            'protocols': [],
        },
    },
    'backup': {
        'create': {
            'type': 'docker',
            'image': '',
            'system': true,
            'entrypoint': '',
            'args': [],
            'mounts': { },
            'io-format': DockerIoFormat.Yaml,
            'inject': false,
            'shm-size': '',
        },
        'restore': {
            'type': 'docker',
            'image': '',
            'system': true,
            'entrypoint': '',
            'args': [],
            'mounts': { },
            'io-format': DockerIoFormat.Yaml,
            'inject': false,
            'shm-size': '',
        },
    },
    'migrations': null,
    'actions': {
        'resync': {
            'name': 'Resync Blockchain',
            'description': 'Use this to resync the Bitcoin blockchain from genesis',
            'warning': 'This will take a couple of days.',
            'allowed-statuses': [
              PackageMainStatus.Running,
              PackageMainStatus.Stopped,
            ],
            'implementation': {
              'type': 'docker',
              'image': '',
              'system': true,
              'entrypoint': '',
              'args': [],
              'mounts': { },
              'io-format': DockerIoFormat.Yaml,
              'inject': false,
              'shm-size': '',
            },
            'input-spec': {
              'reason': {
                'type': 'string',
                'name': 'Re-sync Reason',
                'description': 'Your reason for re-syncing. Why are you doing this?',
                'nullable': false,
                'masked': false,
                'copyable': false,
                'pattern': '^[a-zA-Z]+$',
                'pattern-description': 'Must contain only letters.',
              },
              'name': {
                'type': 'string',
                'name': 'Your Name',
                'description': 'Tell the class your name.',
                'nullable': true,
                'masked': false,
                'copyable': false,
                'pattern': null,
                'pattern-description': null,
                'warning': 'You may loose all your money by providing your name.',
              },
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
                  'value-names': {
                    'email': 'Email',
                    'text': 'Text',
                    'call': 'Call',
                    'push': 'Push',
                    'webhook': 'Webhook',
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
              'days-ago': {
                'type': 'number',
                'name': 'Days Ago',
                'description': 'Number of days to re-sync.',
                'nullable': false,
                'default': 100,
                'range': '[0, 9999]',
                'integral': true,
              },
              'top-speed': {
                'type': 'number',
                'name': 'Top Speed',
                'description': 'The fastest you can possibly run.',
                'nullable': false,
                'default': null,
                'range': '[-1000, 1000]',
                'integral': false,
                'units': 'm/s',
              },
              'testnet': {
                'name': 'Testnet',
                'type': 'boolean',
                'description': 'determines whether your node is running on testnet or mainnet',
                'warning': 'Chain will have to resync!',
                'default': false,
              },
              'randomEnum': {
                'name': 'Random Enum',
                'type': 'enum',
                'value-names': {
                  'null': 'Null',
                  'good': 'Good',
                  'bad': 'Bad',
                  'ugly': 'Ugly',
                },
                'default': 'null',
                'description': 'This is not even real.',
                'warning': 'Be careful changing this!',
                'values': [
                  'null',
                  'good',
                  'bad',
                  'ugly',
                ],
              },
              'emergency-contact': {
                'name': 'Emergency Contact',
                'type': 'object',
                'unique-by': null,
                'description': 'The person to contact in case of emergency.',
                'spec': {
                  'name': {
                    'type': 'string',
                    'name': 'Name',
                    'description': null,
                    'nullable': false,
                    'masked': false,
                    'copyable': false,
                    'pattern': '^[a-zA-Z]+$',
                    'pattern-description': 'Must contain only letters.',
                  },
                  'email': {
                    'type': 'string',
                    'name': 'Email',
                    'description': null,
                    'nullable': false,
                    'masked': false,
                    'copyable': true,
                  },
                },
              },
              'ips': {
                'name': 'Whitelist IPs',
                'type': 'list',
                'subtype': 'string',
                'description': 'external ip addresses that are authorized to access your Bitcoin node',
                'warning': 'Any IP you allow here will have RPC access to your Bitcoin node.',
                'range': '[1,10]',
                'default': [
                  '192.168.1.1',
                ],
                'spec': {
                  'pattern': '^[0-9]{1,3}([,.][0-9]{1,3})?$',
                  'pattern-description': 'Must be a valid IP address',
                  masked: false,
                  copyable: false,
                },
              },
              'bitcoinNode': {
                'name': 'Bitcoin Node Settings',
                'type': 'union',
                'unique-by': null,
                'description': 'The node settings',
                'default': 'internal',
                'warning': 'Careful changing this',
                'tag': {
                    'id': 'type',
                    'name': 'Type',
                    'variant-names': {
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
                    'friendly-name': {
                      'name': 'Friendly Name',
                      'type': 'string',
                      'description': 'the lan address',
                      'nullable': true,
                      'masked': false,
                      'copyable': false,
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
                      'pattern-description': 'anything',
                      'masked': false,
                      'copyable': true,
                    },
                  },
                },
              },
          },
        },
    },
    'permissions': { },
    'dependencies': { },
  }

  export const MockManifestLnd: Manifest = {
    'id': 'lnd',
    'title': 'Lightning Network Daemon',
    'version': '0.11.1',
    'description': {
        'short': 'A bolt spec compliant client.',
        'long': 'More info about LND. More info about LND. More info about LND.',
    },
    'release-notes': 'Dual funded channels!',
    'license': 'MIT',
    'wrapper-repo': 'https://github.com/start9labs/lnd-wrapper',
    'upstream-repo': 'https://github.com/lightningnetwork/lnd',
    'support-site': 'https://lightning.engineering/',
    'marketing-site': 'https://lightning.engineering/',
    'donation-url': null,
    'alerts': {
        'install': null,
        'uninstall': null,
        'restore': 'If this is a duplicate instance of the same LND node, you may loose your funds.',
        'start': 'Starting LND is good for your health.',
        'stop': null,
    },
    'main': {
        'type': 'docker',
        'image': '',
        'system': true,
        'entrypoint': '',
        'args': [],
        'mounts': { },
        'io-format': DockerIoFormat.Yaml,
        'inject': false,
        'shm-size': '',
    },
    'health-checks': { },
    'config': {
      'get': null,
      'set': null,
    },
    'volumes': { },
    'min-os-version': '0.2.12',
    'interfaces': {
        'rpc': {
            'name': 'RPC interface',
            'description': 'Good for connecting to your node at a distance.',
            'ui': true,
            'tor-config': {
                'port-mapping': { },
            },
            'lan-config': {
                '44': {
                    'ssl': true,
                    'mapping': 33,
                },
            },
            'protocols': [],
        },
        'grpc': {
            'name': 'GRPC',
            'description': 'Certain wallet use grpc.',
            'ui': false,
            'tor-config': {
                'port-mapping': { },
            },
            'lan-config': {
                '66': {
                    'ssl': true,
                    'mapping': 55,
                },
            },
            'protocols': [],
        },
    },
    'backup': {
        'create': {
            'type': 'docker',
            'image': '',
            'system': true,
            'entrypoint': '',
            'args': [],
            'mounts': { },
            'io-format': DockerIoFormat.Yaml,
            'inject': false,
            'shm-size': '',
        },
        'restore': {
            'type': 'docker',
            'image': '',
            'system': true,
            'entrypoint': '',
            'args': [],
            'mounts': { },
            'io-format': DockerIoFormat.Yaml,
            'inject': false,
            'shm-size': '',
        },
    },
    'migrations': null,
    'actions': {
        'resync': {
            'name': 'Resync Network Graph',
            'description': 'Your node will resync its network graph.',
            'warning': 'This will take a couple hours.',
            'allowed-statuses': [
                PackageMainStatus.Running,
            ],
            'implementation': {
                'type': 'docker',
                'image': '',
                'system': true,
                'entrypoint': '',
                'args': [],
                'mounts': { },
                'io-format': DockerIoFormat.Yaml,
                'inject': false,
                'shm-size': '',
            },
            'input-spec': null,
        },
    },
    'permissions': { },
    'dependencies': {
        'bitcoind': {
            'version': '=0.21.0',
            'description': 'LND needs bitcoin to live.',
            'requirement': {
              'type': 'opt-out',
              'how': 'You can use an external node from your Embassy if you prefer.',
            },
            'config': null,
            'critical': true,
        },
        'btc-rpc-proxy': {
            'version': '>=0.2.2',
            'description': 'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
            'requirement': {
              'type': 'opt-in',
              'how': 'To use Proxy\'s user management system, go to LND config and select Bitcoin Proxy under Bitcoin config.',
            },
            'config': null,
            'critical': true,
        },
    },
  }

  export const MockManifestBitcoinProxy: Manifest = {
    id: 'btc-rpc-proxy',
    title: 'Bitcoin Proxy',
    version: '0.2.2',
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
      install: null,
      uninstall: null,
      restore: null,
      start: null,
      stop: null,
    },
    main: {
      type: 'docker',
      image: '',
      system: true,
      entrypoint: '',
      args: [''],
      mounts: { },
      'io-format': DockerIoFormat.Yaml,
      inject: false,
      'shm-size': '',
    },
    'health-checks': { },
    config: { get: { } as any, set: { } as any },
    volumes: { },
    'min-os-version': '0.2.12',
    interfaces: {
      rpc: {
        name: 'RPC interface',
        description: 'Good for connecting to your node at a distance.',
        ui: false,
        'tor-config': {
          'port-mapping': { },
        },
        'lan-config': {
          44: {
            ssl: true,
            mapping: 33,
          },
        },
        protocols: [],
      },
    },
    backup: {
      create: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: { },
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
      restore: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: { },
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
      },
    },
    migrations: null,
    actions: { },
    permissions: { },
    dependencies: {
      'bitcoind': {
        version: '>=0.20.0',
        description: 'Bitcoin Proxy requires a Bitcoin node.',
        requirement: {
          type: 'required',
        },
        critical: false,
        config: {
          check: {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'true',
            args: [],
            mounts: { },
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            'shm-size': '10m',
          },
          'auto-configure': {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'cat',
            args: [],
            mounts: { },
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            'shm-size': '10m',
          },
        },
      },
    },
  }

  export const MarketplacePkgs: {
    [id: string]: {
      [version: string]: MarketplacePkg
    }
  } = {
    'bitcoind': {
      '0.19.2': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.19.0',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': { },
      },
      '0.20.0': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': { },
      },
      '0.21.0': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.21.0',
          'release-notes': 'For a complete list of changes, please visit <a href="https://bitcoincore.org/en/releases/0.21.0/">https://bitcoincore.org/en/releases/0.21.0/</a><br /><ul><li>Taproot!</li><li>New RPCs</li><li>Experimental Descriptor Wallets</li></ul>',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': { },
      },
      'latest': {
        icon: 'assets/img/service-icons/bitcoind.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          'release-notes': 'For a complete list of changes, please visit <a href="https://bitcoincore.org/en/releases/0.21.0/">https://bitcoincore.org/en/releases/0.21.0/</a><br /><ul><li>Taproot!</li><li>New RPCs</li><li>Experimental Descriptor Wallets</li></ul>',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': { },
      },
    },
    'lnd': {
      '0.11.0': {
        icon: 'assets/img/service-icons/lnd.png',
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
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
          'btc-rpc-proxy': {
            title: 'Bitcoin Proxy',
            icon: 'assets/img/service-icons/btc-rpc-proxy.png',
          },
        },
      },
      '0.11.1': {
        icon: 'assets/img/service-icons/lnd.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.1',
          'release-notes': 'release notes for LND 0.11.0',
        },
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
          'btc-rpc-proxy': {
            title: 'Bitcoin Proxy',
            icon: 'assets/img/service-icons/btc-rpc-proxy.png',
          },
        },
      },
      'latest': {
        icon: 'assets/img/service-icons/lnd.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestLnd,
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
          'btc-rpc-proxy': {
            title: 'Bitcoin Proxy',
            icon: 'assets/img/service-icons/btc-rpc-proxy.png',
          },
        },
      },
    },
    'btc-rpc-proxy': {
      'latest': {
        icon: 'assets/img/service-icons/btc-rpc-proxy.png',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestBitcoinProxy,
        categories: ['bitcoin'],
        versions: ['0.2.2'],
        'dependency-metadata': {
          'bitcoind': {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
        },
      },
    },
  }

  export const MarketplacePkgsList: RR.GetMarketplacePackagesRes = Object.values(Mock.MarketplacePkgs).map(service => service['latest'])

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
          attempted: true,
          error: null,
        },
        packages: {
          'bitcoind': {
            error: null,
          },
        },
      },
    },
    {
      id: 2,
      'package-id': 'bitcoind',
      'created-at': '2019-12-26T14:20:30.872Z',
      code: 2,
      level: NotificationLevel.Warning,
      title: 'SSH Key Added',
      message: 'A new SSH key was added. If you did not do this, shit is bad.',
      data: null,
    },
    {
      id: 3,
      'package-id': 'bitcoind',
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
      message: 'Bitcoind has crashed.',
      data: null,
    },
  ]

  export function getServerMetrics () {
    return {
      'Group1': {
        'Metric1': {
          value: Math.random(),
          unit: 'mi/b',
        },
        'Metric2': {
          value: Math.random(),
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
      'Group3': {
        'Hmmmm1': {
          value: Math.random(),
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
      'Group4': {
        'Hmmmm1': {
          value: Math.random(),
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
      'Group5': {
        'Hmmmm1': {
          value: Math.random(),
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
        'Hmmmm4': {
          value: Math.random(),
          unit: 'mi/b',
        },
        'Hmmmm5': {
          value: 50,
          unit: '%',
        },
        'Hmmmm6': {
          value: 10.1,
          unit: '%',
        },
      },
    }
  }

  export function getAppMetrics () {
    const metr: Metric = {
      'Metric1': {
        value: Math.random(),
        unit: 'mi/b',
      },
      'Metric2': {
        value: Math.random(),
        unit: '%',
      },
      'Metric3': {
        value: 10.1,
        unit: '%',
      },
    }

    return metr
  }

  export const ServerLogs: Log[] = [
    {
      timestamp: '2019-12-26T14:20:30.872Z',
      message: '****** START *****',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      message: 'ServerLogs ServerLogs ServerLogs ServerLogs ServerLogs',
    },
    {
      timestamp: '2019-12-26T14:22:30.872Z',
      message: '****** FINISH *****',
    },
  ]

  export const PackageLogs: Log[] = [
    {
      timestamp: '2019-12-26T14:20:30.872Z',
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
      'b7b1a9cef4284f00af9e9dda6e676177': {
        'last-active': '2021-06-14T20:49:17.774Z',
        'user-agent': 'Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0',
        metadata: {
          platforms: ['desktop'],
        },
      },
    },
  }

  export const ActionResponse: RR.ExecutePackageActionRes = {
    message: 'Password changed successfully. If you lose your new password, you will be lost forever.',
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
    ssids: ['Goosers', 'Goosers5G'],
    connected: 'Goosers',
    country: 'US',
    'signal-strength': 50,
  }

  export const Drives: RR.GetDrivesRes = [
    {
      logicalname: '/dev/sda',
      model: null,
      vendor: 'SSK',
      partitions: [
        {
          logicalname: 'sdba1',
          label: 'Matt Stuff',
          capacity: 1000000000000,
          used: 0,
          'embassy-os': null,
        },
      ],
      capacity: 1000000000000,
    },
    {
      logicalname: '/dev/sdb',
      model: 'JMS567 SATA 6Gb/s bridge',
      vendor: 'Samsung',
      partitions: [
        {
          logicalname: 'sdba1',
          label: 'Partition 1',
          capacity: 1000000000,
          used: 1000000000,
          'embassy-os': {
            version: '0.3.0',
            full: true,
          },
        },
        {
          logicalname: 'sdba2',
          label: 'Partition 2',
          capacity: 900000000,
          used: 300000000,
          'embassy-os': null,
        },
      ],
      capacity: 10000000000,
    },
  ]

  export const BackupInfo: RR.GetBackupInfoRes = {
    version: '0.3.0',
    timestamp: new Date().toISOString(),
    'package-backups': {
      bitcoind: {
        version: '0.21.0',
        'os-version': '0.3.0',
        timestamp: new Date().toISOString(),
      },
    },
  }

  export const PackageProperties: RR.GetPackagePropertiesRes<2> = {
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
      'Another Value': {
        type: 'string',
        description: 'Some more information about the service.',
        copyable: false,
        qr: true,
        masked: false,
        value: 'https://guessagain.com',
      },
    },
  } as any // @TODO why is this necessary?

  export const PackageConfig: RR.GetPackageConfigRes = {
    // config spec
    spec: {
      'testnet': {
        'name': 'Testnet',
        'type': 'boolean',
        'description': 'determines whether your node is running on testnet or mainnet',
        'warning': 'Chain will have to resync!',
        'default': true,
      },
      'object-list': {
        'name': 'Object List',
        'type': 'list',
        'subtype': 'object',
        'description': 'This is a list of objects, like users or something',
        'range': '[0,4]',
        'default': [
          {
            'first-name': 'Admin',
            'last-name': 'User',
            'age': 40,
          },
          {
            'first-name': 'Admin2',
            'last-name': 'User',
            'age': 40,
          },
        ],
        // the outer spec here, at the list level, says that what's inside (the inner spec) pertains to its inner elements.
        // it just so happens that ValueSpecObject's have the field { spec: ConfigSpec }
        // see 'union-list' below for a different example.
        'spec': {
          'unique-by': 'last-name',
          'display-as': `I'm {{last-name}}, {{first-name}} {{last-name}}`,
          'spec': {
            'first-name': {
              'name': 'First Name',
              'type': 'string',
              'description': 'User first name',
              'nullable': true,
              'default': null,
              'masked': false,
              'copyable': false,
            },
            'last-name': {
              'name': 'Last Name',
              'type': 'string',
              'description': 'User first name',
              'nullable': true,
              'default': {
                'charset': 'a-g,2-9',
                'len': 12,
              },
              'pattern': '^[a-zA-Z]+$',
              'pattern-description': 'must contain only letters.',
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
              'warning': 'User must be at least 18.',
              'range': '[18,*)',
            },
          },
        },
      },
      'union-list': {
        'name': 'Union List',
        'type': 'list',
        'subtype': 'union',
        'description': 'This is a sample list of unions',
        'warning': 'If you change this, things may work.',
        // a list of union selections. e.g. 'summer', 'winter',...
        'default': [
          'summer',
        ],
        'range': '[0, 2]',
        'spec': {
          'tag': {
            'id': 'preference',
            'name': 'Preferences',
            'variant-names': {
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
                'value-names': {
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
          'unique-by': 'preference',
        },
      },
      'random-enum': {
        'name': 'Random Enum',
        'type': 'enum',
        'value-names': {
          'null': 'Null',
          'option1': 'One 1',
          'option2': 'Two 2',
          'option3': 'Three 3',
        },
        'default': 'null',
        'description': 'This is not even real.',
        'warning': 'Be careful changing this!',
        'values': [
          'null',
          'option1',
          'option2',
          'option3',
        ],
      },
      'favorite-number': {
        'name': 'Favorite Number',
        'type': 'number',
        'integral': false,
        'description': 'Your favorite number of all time',
        'warning': 'Once you set this number, it can never be changed without severe consequences.',
        'nullable': true,
        'default': 7,
        'range': '(-100,100]',
        'units': 'BTC',
      },
      'unlucky-numbers': {
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
        'unique-by': null,
        'description': 'rpc username and password',
        'warning': 'Adding RPC users gives them special permissions on your node.',
        'spec': {
          'laws': {
            'name': 'Laws',
            'type': 'object',
            'unique-by': 'law1',
            'description': 'the law of the realm',
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
              'unique-by': null,
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
                  'pattern-description': 'may only contain numbers and periods',
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
            'pattern-description': 'must contain only letters.',
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
        'unique-by': null,
        'description': 'Advanced settings',
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
              'value-names': {
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
      'bitcoin-node': {
        'name': 'Bitcoin Node Settings',
        'type': 'union',
        'unique-by': null,
        'description': 'The node settings',
        'default': 'internal',
        'warning': 'Careful changing this',
        'tag': {
          'id': 'type',
          'name': 'Type',
          'variant-names': {
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
              'pattern-description': 'anything',
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
        'range': '(0, 9998]',
      },
      'favorite-slogan': {
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
        'warning': 'Any IP you allow here will have RPC access to your Bitcoin node.',
        'range': '[1,10]',
        'default': [
          '192.168.1.1',
        ],
        'spec': {
          'masked': false,
          'copyable': false,
          'pattern': '((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))',
          'pattern-description': 'must be a valid ipv4, ipv6, or domain name',
        },
      },
      'rpcauth': {
        'name': 'RPC Auth',
        'type': 'list',
        'subtype': 'string',
        'description': 'api keys that are authorized to access your Bitcoin node.',
        'range': '[0,*)',
        'default': [],
        'spec': {
          'masked': false,
          'copyable': false,
        },
      },
    },
    // actual config
    config: {
      testnet: false,
      'object-list': [
        {
          'first-name': 'Admin',
          'last-name': 'User',
          'age': 40,
        },
        {
          'first-name': 'Admin2',
          'last-name': 'User',
          'age': 40,
        },
      ],
      'union-list': undefined,
      'random-enum': 'option1',
      'favorite-number': null,
      'secondary-numbers': undefined,
      rpcsettings: {
        laws: {
          law1: 'The first law',
          law2: 'The second law',
        },
        rpcpass: null,
        rpcuser: '123',
        rulemakers: [],
      },
      advanced: {
        notifications: ['email'],
      },
      'bitcoin-node': undefined,
      port: 5959,
      rpcallowip: undefined,
      rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
    },
  }

  export const mockCupsDependentConfig = {
    'random-enum': 'option1',
    testnet: false,
    'favorite-number': 8,
    'secondary-numbers': [13, 58, 20],
    'object-list': [],
    'union-list': [],
    rpcsettings: {
      laws: null,
      rpcpass: null,
      rpcuser: '123',
      rulemakers: [],
    },
    advanced: {
      notifications: [],
    },
    'bitcoin-Node': { type: 'internal' },
    port: 5959,
    rpcallowip: [],
    rpcauth: ['matt: 8273gr8qwoidm1uid91jeh8y23gdio1kskmwejkdnm'],
  }

  export const bitcoind: PackageDataEntry = {
    state: PackageState.Installed,
    'static-files': {
      'license': '/public/package-data/bitcoind/0.20.0/LICENSE.md',
      'icon': '/assets/img/service-icons/bitcoind.png',
      'instructions': '/public/package-data/bitcoind/0.20.0/INSTRUCTIONS.md',
    },
    manifest: MockManifestBitcoind,
    installed: {
      manifest: MockManifestBitcoind,
      'last-backup': null,
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Running,
          started: new Date().toISOString(),
          health: { },
        },
        'dependency-errors': { },
      },
      'interface-addresses': {
        rpc: {
          'tor-address': 'bitcoinproxy-rpc-address.onion',
          'lan-address': 'bitcoinproxy-rpc-address.local',
        },
      },
      'system-pointers': [],
      'current-dependents': {
        'lnd': {
          pointers: [],
          'health-checks': [],
        },
      },
      'current-dependencies': { },
      'dependency-info': { },
    },
    'install-progress': undefined,
  }

  export const bitcoinProxy: PackageDataEntry = {
    state: PackageState.Installed,
    'static-files': {
      'license': '/public/package-data/btc-rpc-proxy/0.20.0/LICENSE.md',
      'icon': '/assets/img/service-icons/btc-rpc-proxy.png',
      'instructions': '/public/package-data/btc-rpc-proxy/0.20.0/INSTRUCTIONS.md',
    },
    manifest: MockManifestBitcoinProxy,
    installed: {
      'last-backup': null,
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Running,
          started: new Date().toISOString(),
          health: { },
        },
        'dependency-errors': { },
      },
      manifest: MockManifestBitcoinProxy,
      'interface-addresses': {
        rpc: {
          'tor-address': 'bitcoinproxy-rpc-address.onion',
          'lan-address': 'bitcoinproxy-rpc-address.local',
        },
      },
      'system-pointers': [],
      'current-dependents': {
        'lnd': {
          pointers: [],
          'health-checks': [],
        },
      },
      'current-dependencies': {
        'bitcoind': {
          pointers: [],
          'health-checks': [],
        },
      },
      'dependency-info': {
        'lnd': {
          manifest: Mock.MockManifestLnd,
          icon: 'assets/img/service-icons/lnd.png',
        },
        'bitcoind': {
          manifest: Mock.MockManifestBitcoind,
          icon: 'assets/img/service-icons/bitcoind.png',
        },
      },
    },
    'install-progress': undefined,
  }

  export const lnd: PackageDataEntry = {
    state: PackageState.Installed,
    'static-files': {
      'license': '/public/package-data/lnd/0.20.0/LICENSE.md',
      'icon': '/assets/img/service-icons/lnd.png',
      'instructions': '/public/package-data/lnd/0.20.0/INSTRUCTIONS.md',
    },
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
      manifest: MockManifestLnd,
      'interface-addresses': {
        rpc: {
          'tor-address': 'lnd-rpc-address.onion',
          'lan-address': 'lnd-rpc-address.local',
        },
        grpc: {
          'tor-address': 'lnd-grpc-address.onion',
          'lan-address': 'lnd-grpc-address.local',
        },
      },
      'system-pointers': [],
      'current-dependents': { },
      'current-dependencies': {
        'bitcoind': {
          pointers: [],
          'health-checks': [],
        },
        'btc-rpc-proxy': {
          pointers: [],
          'health-checks': [],
        },
      },
      'dependency-info': {
        'bitcoind': {
          manifest: Mock.MockManifestBitcoind,
          icon: 'assets/img/service-icons/bitcoind.png',
        },
        'btc-rpc-proxy': {
          manifest: Mock.MockManifestBitcoinProxy,
          icon: 'assets/img/service-icons/btc-rpc-proxy.png',
        },
      },
    },
    'install-progress': undefined,
  }

  export const LocalPkgs: { [key: string]: PackageDataEntry } = {
    'bitcoind': bitcoind,
    'btc-rpc-proxy': bitcoinProxy,
    'lnd': lnd,
  }
}
