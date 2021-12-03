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
                      'subtype': 'package',
                      'target': 'lan-address',
                      'description': 'the lan address',
                      'interface': 'tor-address',
                      'package-id': '12341234',
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
        },
        'btc-rpc-proxy': {
            'version': '>=0.2.2',
            'description': 'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
            'requirement': {
              'type': 'opt-in',
              'how': 'To use Proxy\'s user management system, go to LND config and select Bitcoin Proxy under Bitcoin config.',
            },
            'config': null,
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
          attempted: false,
          error: null,
        },
        packages: {
          'bitcoind': {
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
      message: `2021-11-27T18:36:24.049466Z 2021-11-27T18:36:24Z UpdateTip: new best=0000000000000000000b983471c9d97da0493d666aed2182b9f5724d8de882f2 height=655129 version=0x20000000 log2_work=92.414586 tx=583135182 date='2020-11-02T16:29:08Z' progress=0.831387 cache=952.9MiB(7030955txo)

      2021-11-27T18:36:30.451064Z 2021-11-27T18:36:30Z tor: Thread interrupt

      2021-11-27T18:36:30.452833Z 2021-11-27T18:36:30Z Shutdown: In progress...

      2021-11-27T18:36:30.453128Z 2021-11-27T18:36:30Z addcon thread exit

      2021-11-27T18:36:30.453369Z 2021-11-27T18:36:30Z torcontrol thread exit

      2021-11-27T18:36:30.478636Z 2021-11-27T18:36:30Z net thread exit

      2021-11-27T18:36:35.231796Z 2021-11-27T18:36:35Z UpdateTip: new best=00000000000000000006ee6ef3e1e3f5ce77ec364a410d013a3998d0e1fde1c4 height=655130 version=0x27ffe000 log2_work=92.414604 tx=583138309 date='2020-11-02T17:18:08Z' progress=0.831400 cache=953.6MiB(7036877txo)

      2021-11-27T18:36:35.233427Z 2021-11-27T18:36:35Z opencon thread exit

      2021-11-27T18:36:35.895778Z 2021-11-27T18:36:35Z msghand thread exit

      2021-11-27T18:36:36.925087Z 2021-11-27T18:36:36Z DumpAnchors: Flush 1 outbound block-relay-only peer addresses to anchors.dat started

      2021-11-27T18:36:36.937843Z 2021-11-27T18:36:36Z DumpAnchors: Flush 1 outbound block-relay-only peer addresses to anchors.dat completed (0.01s)

      2021-11-27T18:36:38.329495Z 2021-11-27T18:36:38Z scheduler thread exit

      2021-11-27T18:36:38.507800Z 2021-11-27T18:36:38Z Writing 0 unbroadcast transactions to disk.

      2021-11-27T18:36:38.527805Z 2021-11-27T18:36:38Z Dumped mempool: 3.2e-05s to copy, 0.01261s to dump

      2021-11-27T18:36:38.556270Z 2021-11-27T18:36:38Z FlushStateToDisk: write coins cache to disk (7036877 coins, 999924kB) started

      2021-11-27T18:36:40.278299Z Error updating blockchain info: error: Could not connect to the server 127.0.0.1:8332

      2021-11-27T18:36:40.289046Z

      2021-11-27T18:36:40.289645Z Make sure the bitcoind server is running and that you are connecting to the correct RPC port.

      2021-11-27T18:36:40.289913Z

      2021-11-27T18:36:45.290631Z Error updating blockchain info: error: Could not connect to the server 127.0.0.1:8332

      2021-11-27T18:36:45.292144Z

      2021-11-27T18:36:45.292426Z Make sure the bitcoind server is running and that you are connecting to the correct RPC port.

      2021-11-27T18:36:45.292664Z

      2021-11-27T18:36:50.316513Z Error updating blockchain info: error: Could not connect to the server 127.0.0.1:8332

      2021-11-27T18:36:50.318147Z

      2021-11-27T18:36:50.318520Z Make sure the bitcoind server is running and that you are connecting to the correct RPC port.

      2021-11-27T18:36:50.318806Z

      2021-11-27T18:36:55.354490Z Error updating blockchain info: error: Could not connect to the server 127.0.0.1:8332

      2021-11-27T18:36:55.356816Z

      2021-11-27T18:36:55.357236Z Make sure the bitcoind server is running and that you are connecting to the correct RPC port.

      2021-11-27T18:36:55.357570Z

      2021-11-27T18:37:00.400082Z Error updating blockchain info: error: Could not connect to the server 127.0.0.1:8332

      2021-11-27T18:37:00.400690Z

      2021-11-27T18:37:00.400992Z Make sure the bitcoind server is running and that you are connecting to the correct RPC port.

      2021-11-27T18:37:00.401234Z

      ------//------
      Logs after restarting:
      2021-11-27T18:40:34.243380Z Error updating blockchain info: error: Could not connect to the server 127.0.0.1:8332

      2021-11-27T18:40:34.244792Z

      2021-11-27T18:40:34.245168Z Make sure the bitcoind server is running and that you are connecting to the correct RPC port.

      2021-11-27T18:40:34.245545Z

      2021-11-27T18:40:34.678358Z 2021-11-27T18:40:34Z Bitcoin Core version v0.21.1.0-g194b9b8792d9b0798fdb570b79fa51f1d1f5ebaf (release build)

      2021-11-27T18:40:34.678983Z 2021-11-27T18:40:34Z InitParameterInteraction: parameter interaction: -externalip set -> setting -discover=0

      2021-11-27T18:40:34.679277Z 2021-11-27T18:40:34Z Assuming ancestors of block 0000000000000000000b9d2ec5a352ecba0592946514a92f14319dc2b367fc72 have valid signatures.

      2021-11-27T18:40:34.679505Z 2021-11-27T18:40:34Z Setting nMinimumChainWork=00000000000000000000000000000000000000001533efd8d716a517fe2c5008

      2021-11-27T18:40:34.679706Z 2021-11-27T18:40:34Z Using the 'standard' SHA256 implementation

      2021-11-27T18:40:34.679898Z 2021-11-27T18:40:34Z Default data directory /root/.bitcoin

      2021-11-27T18:40:34.680144Z 2021-11-27T18:40:34Z Using data directory /root/.bitcoin

      2021-11-27T18:40:34.680373Z 2021-11-27T18:40:34Z Config file: /root/.bitcoin/bitcoin.conf

      2021-11-27T18:40:34.680569Z 2021-11-27T18:40:34Z Config file arg: avoidpartialspends="1"

      2021-11-27T18:40:34.680750Z 2021-11-27T18:40:34Z Config file arg: bind="0.0.0.0:8333"

      2021-11-27T18:40:34.680937Z 2021-11-27T18:40:34Z Config file arg: dbcache="2048"

      2021-11-27T18:40:34.681164Z 2021-11-27T18:40:34Z Config file arg: disablewallet="0"

      2021-11-27T18:40:34.681351Z 2021-11-27T18:40:34Z Config file arg: discardfee="0.0001"

      2021-11-27T18:40:34.681555Z 2021-11-27T18:40:34Z Config file arg: listen="1"

      2021-11-27T18:40:34.681741Z 2021-11-27T18:40:34Z Config file arg: maxmempool="300"

      2021-11-27T18:40:34.681956Z 2021-11-27T18:40:34Z Config file arg: mempoolexpiry="336"

      2021-11-27T18:40:34.682168Z 2021-11-27T18:40:34Z Config file arg: persistmempool="1"

      2021-11-27T18:40:34.682380Z 2021-11-27T18:40:34Z Config file arg: rpcallowip="0.0.0.0/0"

      2021-11-27T18:40:34.684147Z 2021-11-27T18:40:34Z Config file arg: rpcbind=****

      2021-11-27T18:40:34.690602Z 2021-11-27T18:40:34Z Config file arg: rpcpassword=****

      2021-11-27T18:40:34.691038Z 2021-11-27T18:40:34Z Config file arg: rpcserialversion="1"

      2021-11-27T18:40:34.691281Z 2021-11-27T18:40:34Z Config file arg: rpcservertimeout="30"

      2021-11-27T18:40:34.691610Z 2021-11-27T18:40:34Z Config file arg: rpcthreads="1"

      2021-11-27T18:40:34.691903Z 2021-11-27T18:40:34Z Config file arg: rpcuser=****

      2021-11-27T18:40:34.692101Z 2021-11-27T18:40:34Z Config file arg: rpcworkqueue="16"

      2021-11-27T18:40:34.692293Z 2021-11-27T18:40:34Z Config file arg: whitelist="172.18.0.0/16"

      2021-11-27T18:40:34.692476Z 2021-11-27T18:40:34Z Config file arg: zmqpubrawblock="tcp://0.0.0.0:28332"

      2021-11-27T18:40:34.692711Z 2021-11-27T18:40:34Z Config file arg: zmqpubrawtx="tcp://0.0.0.0:28333"

      2021-11-27T18:40:34.692907Z 2021-11-27T18:40:34Z Command-line arg: conf="/root/.bitcoin/bitcoin.conf"

      2021-11-27T18:40:34.693121Z 2021-11-27T18:40:34Z Command-line arg: datadir="/root/.bitcoin"

      2021-11-27T18:40:34.693308Z 2021-11-27T18:40:34Z Command-line arg: externalip="qc2kmuxdpns7pbjwfffmbeu6rgy3rmkt5w6cuaid34m776kaxfwwdcqd.onion"

      2021-11-27T18:40:34.693526Z 2021-11-27T18:40:34Z Command-line arg: onion="bitcoind.embassy:9050"

      2021-11-27T18:40:34.693714Z 2021-11-27T18:40:34Z Using at most 125 automatic connections (1048576 file descriptors available)

      2021-11-27T18:40:34.749113Z 2021-11-27T18:40:34Z Using 16 MiB out of 32/2 requested for signature cache, able to store 524288 elements

      2021-11-27T18:40:34.807657Z 2021-11-27T18:40:34Z Using 16 MiB out of 32/2 requested for script execution cache, able to store 524288 elements

      2021-11-27T18:40:34.809293Z 2021-11-27T18:40:34Z Script verification uses 3 additional threads

      2021-11-27T18:40:34.820032Z 2021-11-27T18:40:34Z scheduler thread start

      2021-11-27T18:40:34.877830Z 2021-11-27T18:40:34Z WARNING: the RPC server is not safe to expose to untrusted networks such as the public internet

      2021-11-27T18:40:34.878229Z 2021-11-27T18:40:34Z HTTP: creating work queue of depth 16

      2021-11-27T18:40:34.878503Z 2021-11-27T18:40:34Z Config options rpcuser and rpcpassword will soon be deprecated. Locally-run instances may remove rpcuser to use cookie-based auth, or may be replaced with rpcauth. Please see share/rpcauth for rpcauth auth generation.

      2021-11-27T18:40:34.878738Z 2021-11-27T18:40:34Z HTTP: starting 1 worker threads

      2021-11-27T18:40:34.880090Z 2021-11-27T18:40:34Z Using wallet directory /root/.bitcoin

      2021-11-27T18:40:34.880616Z 2021-11-27T18:40:34Z init message: Verifying wallet(s)...

      2021-11-27T18:40:34.881764Z 2021-11-27T18:40:34Z init message: Loading banlist...

      2021-11-27T18:40:34.891297Z 2021-11-27T18:40:34Z SetNetworkActive: true

      2021-11-27T18:40:34.899673Z 2021-11-27T18:40:34Z AddLocal(qc2kmuxdpns7pbjwfffmbeu6rgy3rmkt5w6cuaid34m776kaxfwwdcqd.onion:8333,4)

      2021-11-27T18:40:34.900099Z 2021-11-27T18:40:34Z Using /16 prefix for IP bucketing

      2021-11-27T18:40:34.901498Z 2021-11-27T18:40:34Z Cache configuration:

      2021-11-27T18:40:34.901815Z 2021-11-27T18:40:34Z * Using 2.0 MiB for block index database

      2021-11-27T18:40:34.902038Z 2021-11-27T18:40:34Z * Using 8.0 MiB for chain state database

      2021-11-27T18:40:34.902245Z 2021-11-27T18:40:34Z * Using 2038.0 MiB for in-memory UTXO set (plus up to 286.1 MiB of unused mempool space)

      2021-11-27T18:40:34.902457Z 2021-11-27T18:40:34Z init message: Loading block index...

      2021-11-27T18:40:34.902635Z 2021-11-27T18:40:34Z Switching active chainstate to Chainstate [ibd] @ height -1 (null)

      2021-11-27T18:40:34.902839Z 2021-11-27T18:40:34Z Opening LevelDB in /root/.bitcoin/blocks/index

      2021-11-27T18:40:34.952271Z 2021-11-27T18:40:34Z Opened LevelDB successfully

      2021-11-27T18:40:34.952654Z 2021-11-27T18:40:34Z Using obfuscation key for /root/.bitcoin/blocks/index: 0000000000000000

      2021-11-27T18:40:39.284399Z Error updating blockchain info: error code: -28

      2021-11-27T18:40:39.284933Z error message:

      2021-11-27T18:40:39.285236Z Loading block index...

      2021-11-27T18:40:39.285572Z

      2021-11-27T18:40:44.325367Z Error updating blockchain info: error code: -28

      2021-11-27T18:40:44.325882Z error message:

      2021-11-27T18:40:44.326147Z Loading block index...

      2021-11-27T18:40:44.326360Z

      2021-11-27T18:40:49.349655Z Error updating blockchain info: error code: -28

      2021-11-27T18:40:49.350902Z error message:

      2021-11-27T18:40:49.351237Z Loading block index...

      2021-11-27T18:40:49.351585Z

      2021-11-27T18:40:51.716756Z 2021-11-27T18:40:51Z LoadBlockIndexDB: last block file = 2303

      2021-11-27T18:40:51.805720Z 2021-11-27T18:40:51Z LoadBlockIndexDB: last block file info: CBlockFileInfo(blocks=84, size=109038969, heights=655105...655274, time=2020-11-02...2020-11-03)

      2021-11-27T18:40:51.806295Z 2021-11-27T18:40:51Z Checking all blk files are present...

      2021-11-27T18:40:52.673629Z 2021-11-27T18:40:52Z Opening LevelDB in /root/.bitcoin/chainstate

      2021-11-27T18:40:54.376811Z Error updating blockchain info: error code: -28

      2021-11-27T18:40:54.378172Z error message:

      2021-11-27T18:40:54.378488Z Loading block index...

      2021-11-27T18:40:54.378806Z

      2021-11-27T18:40:55.360312Z 2021-11-27T18:40:55Z Opened LevelDB successfully

      2021-11-27T18:40:55.390938Z 2021-11-27T18:40:55Z Using obfuscation key for /root/.bitcoin/chainstate: 6731b1652378c561

      2021-11-27T18:40:55.393821Z 2021-11-27T18:40:55Z Replaying blocks

      2021-11-27T18:40:55.394345Z 2021-11-27T18:40:55Z Rolling forward 00000000000000000001f4a3b8eb21beb2c90b4212cf103a90c1729546df81b0 (652597)

      2021-11-27T18:40:59.408025Z Error updating blockchain info: error code: -28

      2021-11-27T18:40:59.409326Z error message:

      2021-11-27T18:40:59.409632Z Loading block index...

      2021-11-27T18:40:59.409876Z

      2021-11-27T18:41:04.438347Z Error updating blockchain info: error code: -28

      2021-11-27T18:41:04.440537Z error message:

      2021-11-27T18:41:04.441003Z Loading block index...

      2021-11-27T18:41:04.441406Z`,
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
      message: '2021/11/09 22:55:04 \u001b[0;32;49mPOST \u001b[0;32;49m200\u001b[0m photoview.embassy/api/graphql \u001b[0;36;49m1.169406ms\u001b[0m unauthenticated',
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
      guid: 'asdfasdf',
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
      guid: null,
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

  const realWorldConfigSpec: RR.GetPackageConfigRes = {
    'config': {
       'bitcoind': {
          'bitcoind-rpc': {
             'type': 'internal',
             'rpc-user': 'btcpayserver',
             'rpc-password': '3ZRIgPq60y3bpWRPXKPdhV',
          },
          'bitcoind-p2p': {
             'type': 'internal',
          },
       },
       'lightning': {
          'type': 'lnd',
       },
       'tor-address': 'fzo2uwpfx3yxgvb2fjhmkbxa27logah3c7zw3fc6rxq5osqyegyy34qd.onion',
    },
    'spec': {
       'tor-address': {
          'type': 'pointer',
          'subtype': 'package',
          'target': 'tor-address',
          'package-id': 'btcpayserver',
          'interface': 'network',
          'description': 'The Tor address for the network interface.',
          'name': 'Network Tor Address',
       },
       'bitcoind': {
          'type': 'object',
          'spec': {
             'bitcoind-rpc': {
                'type': 'union',
                'tag': {
                   'id': 'type',
                   'name': 'Type',
                   'description': 'The Bitcoin Core node to connect to:\n  - internal: The Bitcoin Core and Proxy services installed to your Embassy\n  - external: An unpruned Bitcoin Core node running on a different device\n',
                   'variant-names': {
                      'external': 'External',
                      'internal': 'Internal',
                   },
                },
                'variants': {
                   'external': {
                      'connection-settings': {
                         'type': 'union',
                         'tag': {
                            'id': 'type',
                            'name': 'Type',
                            'description': '- Manual: Raw information for finding a Bitcoin Core node\n- Quick Connect: A Quick Connect URL for a Bitcoin Core node\n',
                            'variant-names': {
                               'manual': 'Manual',
                               'quick-connect': 'Quick Connect',
                            },
                         },
                         'variants': {
                            'manual': {
                               'rpc-host': {
                                  'type': 'string',
                                  'copyable': false,
                                  'masked': false,
                                  'nullable': false,
                                  'default': null,
                                  'description': 'The public address of your Bitcoin Core server',
                                  'name': 'Public Address',
                               },
                               'rpc-user': {
                                  'type': 'string',
                                  'copyable': false,
                                  'masked': false,
                                  'nullable': false,
                                  'default': null,
                                  'description': 'The username for the RPC user on your Bitcoin Core RPC server',
                                  'name': 'RPC Username',
                               },
                               'rpc-password': {
                                  'type': 'string',
                                  'copyable': false,
                                  'masked': false,
                                  'nullable': false,
                                  'default': null,
                                  'description': 'The password for the RPC user on your Bitcoin Core RPC server',
                                  'name': 'RPC Password',
                               },
                               'rpc-port': {
                                  'type': 'number',
                                  'range': '[0,65535]',
                                  'integral': true,
                                  'nullable': false,
                                  'default': 8332,
                                  'description': 'The port that your Bitcoin Core RPC server is bound to',
                                  'name': 'RPC Port',
                               },
                            },
                            'quick-connect': {
                               'quick-connect-url': {
                                  'type': 'string',
                                  'copyable': false,
                                  'masked': false,
                                  'nullable': false,
                                  'default': null,
                                  'description': 'The Quick Connect URL for your Bitcoin Core RPC server\nNOTE: LND will not accept a .onion url for this option\n',
                                  'name': 'Quick Connect URL',
                               },
                            },
                         },
                         'display-as': null,
                         'unique-by': null,
                         'default': 'quick-connect',
                         'description': 'Information to connect to an external unpruned Bitcoin Core node',
                         'name': 'Connection Settings',
                      },
                   },
                   'internal': {
                      'rpc-user': {
                         'type': 'pointer',
                         'subtype': 'package',
                         'target': 'config',
                         'package-id': 'btc-rpc-proxy',
                         'selector': '$.users.[?(@.name == "btcpayserver")].name',
                         'multi': false,
                         'description': 'The username for the RPC user allocated to BTCPay',
                         'name': 'RPC Username',
                         'interface': 'asdf',
                      },
                      'rpc-password': {
                         'type': 'pointer',
                         'subtype': 'package',
                         'target': 'config',
                         'package-id': 'btc-rpc-proxy',
                         'selector': '$.users.[?(@.name == "btcpayserver")].password',
                         'multi': false,
                         'description': 'The password for the RPC user allocated to BTCPay',
                         'name': 'RPC Password',
                         'interface': 'asdf',
                      },
                   },
                },
                'display-as': null,
                'unique-by': null,
                'default': 'internal',
                'description': 'The Bitcoin Core node to connect to over the RPC interface',
                'name': 'Bitcoin Core RPC',
             },
             'pruning': {
                'default': 'disabled',
                'description': 'Blockchain Pruning Options\nReduce the blockchain size on disk\n',
                'display-as': null,
                'name': 'Pruning Settings',
                'tag': {
                  'description': '- Disabled: Disable pruning\n- Automatic: Limit blockchain size on disk to a certain number of megabytes\n- Manual: Prune blockchain with the "pruneblockchain" RPC\n',
                  'id': 'mode',
                  'name': 'Pruning Mode',
                  'variant-names': {
                    'automatic': 'Automatic',
                    'disabled': 'Disabled',
                    'manual': 'Manual',
                  },
                },
                'type': 'union',
                'unique-by': null,
                'variants': {
                  'automatic': {
                    'size': {
                      'default': 550,
                      'description': 'Limit of blockchain size on disk.',
                      'integral': true,
                      'name': 'Max Chain Size',
                      'nullable': false,
                      'range': '[550,1000000)',
                      'type': 'number',
                      'units': 'MiB',
                    },
                  },
                  'disabled': { },
                  'manual': {
                    'size': {
                        'default': 65536,
                        'description': 'Prune blockchain if size expands beyond this.',
                        'integral': true,
                        'name': 'Failsafe Chain Size',
                        'nullable': false,
                        'range': '[550,1000000)',
                        'type': 'number',
                        'units': 'MiB',
                    },
                  },
                },
              },
             'bitcoind-p2p': {
                'type': 'union',
                'tag': {
                   'id': 'type',
                   'name': 'type',
                   'description': null,
                   'variant-names': {
                      'external': 'external',
                      'internal': 'internal',
                   },
                },
                'variants': {
                   'external': {
                      'p2p-host': {
                         'type': 'string',
                         'copyable': false,
                         'masked': false,
                         'nullable': false,
                         'default': null,
                         'description': 'The public address of your Bitcoin Core server',
                         'name': 'Public Address',
                      },
                      'p2p-port': {
                         'type': 'number',
                         'range': '[0,65535]',
                         'integral': true,
                         'nullable': false,
                         'default': 8333,
                         'description': 'The port that your Bitcoin Core P2P server is bound to',
                         'name': 'P2P Port',
                      },
                   },
                   'internal': {

                   },
                },
                'display-as': null,
                'unique-by': null,
                'default': 'internal',
                'description': 'The Bitcoin Core node to connect to over the P2P interface',
                'name': 'Bitcoin Core P2P',
             },
          },
          'display-as': null,
          'unique-by': null,
          'description': 'RPC and P2P interface configuration options for Bitcoin Core',
          'name': 'Bitcoin Settings',
       },
       'lightning': {
          'type': 'union',
          'tag': {
             'id': 'type',
             'name': 'Type',
             'description': 'Enables BTCPay to use the selected internal lightning node.',
             'variant-names': {
                'c-lightning': 'C-Lightning',
                'lnd': 'LND',
                'none': 'No selection',
             },
          },
          'variants': {
             'c-lightning': {

             },
             'lnd': {

             },
             'none': {

             },
          },
          'display-as': null,
          'unique-by': null,
          'default': 'none',
          'description': 'Use this setting to grant BTCPay access to your Embassy\\\'s LND or c-lightning node. If you prefer to use an external Lightning node, or you do not intend to use Lightning, leave this setting blank. Please see the "Instructions" page for more details.',
          'name': 'Embassy Lightning Node',
       },
    },
 }

  export const ConfigSpec: RR.GetPackageConfigRes['spec'] = realWorldConfigSpec.spec

  const testSpec = {
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
                'subtype': 'package',
                'target': 'lan-address',
                'package-id': 'bitcoind',
                'description': 'the lan address',
                'interface': 'interface',
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
            'subtype': 'package',
            'target': 'lan-address',
            'package-id': 'bitcoind',
            'description': 'the lan address',
            'interface': 'asdf',
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
  }

  export const MockConfig = realWorldConfigSpec.config

  export const MockDependencyConfig = {
    testnet: true,
    'object-list': [
      {
        'first-name': 'First',
        'last-name': 'Last',
        'age': 30,
      },
      {
        'first-name': 'First2',
        'last-name': 'Last2',
        'age': 40,
      },
      {
        'first-name': 'First3',
        'last-name': 'Last3',
        'age': 60,
      },
    ],
    'union-list': undefined,
    'random-enum': 'option2',
    'favorite-number': null,
    rpcsettings: {
      laws: {
        law1: 'The first law Amended',
        law2: 'The second law',
      },
      rpcpass: null,
      rpcuser: '123',
      rulemakers: [],
    },
    advanced: {
      notifications: ['email', 'text', 'push'],
    },
    'bitcoin-node': undefined,
    port: 20,
    rpcallowip: undefined,
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
        configured: false,
        main: {
          status: PackageMainStatus.Stopped,
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
