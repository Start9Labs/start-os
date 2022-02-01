import { ConfigSpec } from 'src/app/pkg-config/config-types'
import {
  DependencyErrorType,
  DockerIoFormat,
  Manifest,
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import {
  Log,
  MarketplacePkg,
  Metric,
  NotificationLevel,
  RR,
  ServerNotifications,
} from './api.types'

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
    '0.19.2':
      'Contrary to popular belief, Lorem Ipsum is not simply random text.',
    '0.19.1': 'release notes for Bitcoin 0.19.1',
    '0.19.0': 'release notes for Bitcoin 0.19.0',
  }

  export const MockManifestBitcoind: Manifest = {
    id: 'bitcoind',
    title: 'Bitcoin Core',
    version: '0.20.0',
    description: {
      short: 'A Bitcoin full node by Bitcoin Core.',
      long: 'Bitcoin is a decentralized consensus protocol and settlement network.',
    },
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
      start: null,
      stop: 'Stopping Bitcoin is bad for your health.',
    },
    main: {
      type: 'docker',
      image: '',
      system: true,
      entrypoint: '',
      args: [],
      mounts: {},
      'io-format': DockerIoFormat.Yaml,
      inject: false,
      'shm-size': '',
      'sigterm-timeout': '1ms',
    },
    'health-checks': {},
    config: {
      get: null,
      set: null,
    },
    volumes: {},
    'min-os-version': '0.2.12',
    interfaces: {
      ui: {
        name: 'Node Visualizer',
        description:
          'Web application for viewing information about your node and the Bitcoin network.',
        ui: true,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {},
        protocols: [],
      },
      rpc: {
        name: 'RPC',
        description: 'Used by wallets to interact with your Bitcoin Core node.',
        ui: false,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {},
        protocols: [],
      },
      p2p: {
        name: 'P2P',
        description:
          'Used by other Bitcoin nodes to communicate and interact with your node.',
        ui: false,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {},
        protocols: [],
      },
    },
    backup: {
      create: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
        'sigterm-timeout': null,
      },
      restore: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
        'sigterm-timeout': null,
      },
    },
    migrations: null,
    actions: {
      resync: {
        name: 'Resync Blockchain',
        description: 'Use this to resync the Bitcoin blockchain from genesis',
        warning: 'This will take a couple of days.',
        'allowed-statuses': [
          PackageMainStatus.Running,
          PackageMainStatus.Stopped,
        ],
        implementation: {
          type: 'docker',
          image: '',
          system: true,
          entrypoint: '',
          args: [],
          mounts: {},
          'io-format': DockerIoFormat.Yaml,
          inject: false,
          'shm-size': '',
          'sigterm-timeout': null,
        },
        'input-spec': {
          reason: {
            type: 'string',
            name: 'Re-sync Reason',
            description: 'Your reason for re-syncing. Why are you doing this?',
            nullable: false,
            masked: false,
            copyable: false,
            pattern: '^[a-zA-Z]+$',
            'pattern-description': 'Must contain only letters.',
          },
          name: {
            type: 'string',
            name: 'Your Name',
            description: 'Tell the class your name.',
            nullable: true,
            masked: false,
            copyable: false,
            pattern: null,
            'pattern-description': null,
            warning: 'You may loose all your money by providing your name.',
          },
          notifications: {
            name: 'Notification Preferences',
            type: 'list',
            subtype: 'enum',
            description: 'how you want to be notified',
            range: '[1,3]',
            default: ['email'],
            spec: {
              'value-names': {
                email: 'Email',
                text: 'Text',
                call: 'Call',
                push: 'Push',
                webhook: 'Webhook',
              },
              values: ['email', 'text', 'call', 'push', 'webhook'],
            },
          },
          'days-ago': {
            type: 'number',
            name: 'Days Ago',
            description: 'Number of days to re-sync.',
            nullable: false,
            default: 100,
            range: '[0, 9999]',
            integral: true,
          },
          'top-speed': {
            type: 'number',
            name: 'Top Speed',
            description: 'The fastest you can possibly run.',
            nullable: false,
            default: null,
            range: '[-1000, 1000]',
            integral: false,
            units: 'm/s',
          },
          testnet: {
            name: 'Testnet',
            type: 'boolean',
            description:
              'determines whether your node is running on testnet or mainnet',
            warning: 'Chain will have to resync!',
            default: false,
          },
          randomEnum: {
            name: 'Random Enum',
            type: 'enum',
            'value-names': {
              null: 'Null',
              good: 'Good',
              bad: 'Bad',
              ugly: 'Ugly',
            },
            default: 'null',
            description: 'This is not even real.',
            warning: 'Be careful changing this!',
            values: ['null', 'good', 'bad', 'ugly'],
          },
          'emergency-contact': {
            name: 'Emergency Contact',
            type: 'object',
            'unique-by': null,
            description: 'The person to contact in case of emergency.',
            spec: {
              name: {
                type: 'string',
                name: 'Name',
                description: null,
                nullable: false,
                masked: false,
                copyable: false,
                pattern: '^[a-zA-Z]+$',
                'pattern-description': 'Must contain only letters.',
              },
              email: {
                type: 'string',
                name: 'Email',
                description: null,
                nullable: false,
                masked: false,
                copyable: true,
              },
            },
          },
          ips: {
            name: 'Whitelist IPs',
            type: 'list',
            subtype: 'string',
            description:
              'external ip addresses that are authorized to access your Bitcoin node',
            warning:
              'Any IP you allow here will have RPC access to your Bitcoin node.',
            range: '[1,10]',
            default: ['192.168.1.1'],
            spec: {
              pattern: '^[0-9]{1,3}([,.][0-9]{1,3})?$',
              'pattern-description': 'Must be a valid IP address',
              masked: false,
              copyable: false,
            },
          },
          bitcoinNode: {
            name: 'Bitcoin Node Settings',
            type: 'union',
            'unique-by': null,
            description: 'The node settings',
            default: 'internal',
            warning: 'Careful changing this',
            tag: {
              id: 'type',
              name: 'Type',
              'variant-names': {
                internal: 'Internal',
                external: 'External',
              },
            },
            variants: {
              internal: {
                'lan-address': {
                  name: 'LAN Address',
                  type: 'pointer',
                  subtype: 'package',
                  target: 'lan-address',
                  description: 'the lan address',
                  interface: 'tor-address',
                  'package-id': '12341234',
                },
                'friendly-name': {
                  name: 'Friendly Name',
                  type: 'string',
                  description: 'the lan address',
                  nullable: true,
                  masked: false,
                  copyable: false,
                },
              },
              external: {
                'public-domain': {
                  name: 'Public Domain',
                  type: 'string',
                  description: 'the public address of the node',
                  nullable: false,
                  default: 'bitcoinnode.com',
                  pattern: '.*',
                  'pattern-description': 'anything',
                  masked: false,
                  copyable: true,
                },
              },
            },
          },
        },
      },
    },
    permissions: {},
    dependencies: {},
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
    main: {
      type: 'docker',
      image: '',
      system: true,
      entrypoint: '',
      args: [],
      mounts: {},
      'io-format': DockerIoFormat.Yaml,
      inject: false,
      'shm-size': '',
      'sigterm-timeout': '10000µs',
    },
    'health-checks': {},
    config: {
      get: null,
      set: null,
    },
    volumes: {},
    'min-os-version': '0.2.12',
    interfaces: {
      rpc: {
        name: 'RPC interface',
        description: 'Good for connecting to your node at a distance.',
        ui: true,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {
          '44': {
            ssl: true,
            mapping: 33,
          },
        },
        protocols: [],
      },
      grpc: {
        name: 'GRPC',
        description: 'Certain wallet use grpc.',
        ui: false,
        'tor-config': {
          'port-mapping': {},
        },
        'lan-config': {
          '66': {
            ssl: true,
            mapping: 55,
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
        args: [],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
        'sigterm-timeout': null,
      },
      restore: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
        'sigterm-timeout': null,
      },
    },
    migrations: null,
    actions: {
      resync: {
        name: 'Resync Network Graph',
        description: 'Your node will resync its network graph.',
        warning: 'This will take a couple hours.',
        'allowed-statuses': [PackageMainStatus.Running],
        implementation: {
          type: 'docker',
          image: '',
          system: true,
          entrypoint: '',
          args: [],
          mounts: {},
          'io-format': DockerIoFormat.Yaml,
          inject: false,
          'shm-size': '',
          'sigterm-timeout': null,
        },
        'input-spec': null,
      },
    },
    permissions: {},
    dependencies: {
      bitcoind: {
        version: '=0.21.0',
        description: 'LND needs bitcoin to live.',
        requirement: {
          type: 'opt-out',
          how: 'You can use an external node from your Embassy if you prefer.',
        },
        config: null,
      },
      'btc-rpc-proxy': {
        version: '>=0.2.2',
        description:
          'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
        requirement: {
          type: 'opt-in',
          how: "To use Proxy's user management system, go to LND config and select Bitcoin Proxy under Bitcoin config.",
        },
        config: null,
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
      mounts: {},
      'io-format': DockerIoFormat.Yaml,
      inject: false,
      'shm-size': '',
      'sigterm-timeout': '1m',
    },
    'health-checks': {},
    config: { get: {} as any, set: {} as any },
    volumes: {},
    'min-os-version': '0.2.12',
    interfaces: {
      rpc: {
        name: 'RPC interface',
        description: 'Good for connecting to your node at a distance.',
        ui: false,
        'tor-config': {
          'port-mapping': {},
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
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
        'sigterm-timeout': null,
      },
      restore: {
        type: 'docker',
        image: '',
        system: true,
        entrypoint: '',
        args: [''],
        mounts: {},
        'io-format': DockerIoFormat.Yaml,
        inject: false,
        'shm-size': '',
        'sigterm-timeout': null,
      },
    },
    migrations: null,
    actions: {},
    permissions: {},
    dependencies: {
      bitcoind: {
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
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            'shm-size': '10m',
            'sigterm-timeout': null,
          },
          'auto-configure': {
            type: 'docker',
            image: 'alpine',
            system: true,
            entrypoint: 'cat',
            args: [],
            mounts: {},
            'io-format': DockerIoFormat.Cbor,
            inject: false,
            'shm-size': '10m',
            'sigterm-timeout': null,
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
    bitcoind: {
      '0.19.2': {
        icon: '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',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.19.0',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
      },
      '0.20.0': {
        icon: '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',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
      },
      '0.21.0': {
        icon: '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',
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
      },
      latest: {
        icon: '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',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: {
          ...Mock.MockManifestBitcoind,
          'release-notes':
            'For a complete list of changes, please visit <a href="https://bitcoincore.org/en/releases/0.21.0/">https://bitcoincore.org/en/releases/0.21.0/</a><br /><ul><li>Taproot!</li><li>New RPCs</li><li>Experimental Descriptor Wallets</li></ul>',
        },
        categories: ['bitcoin', 'cryptocurrency'],
        versions: ['0.19.0', '0.20.0', '0.21.0'],
        'dependency-metadata': {},
      },
    },
    lnd: {
      '0.11.0': {
        icon: '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',
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
          bitcoind: {
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
        icon: 'iVBORw0KGgoAAAANSUhEUgAAAQAAAAEACAYAAABccqhmAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAhGVYSWZNTQAqAAAACAAFARIAAwAAAAEAAQAAARoABQAAAAEAAABKARsABQAAAAEAAABSASgAAwAAAAEAAgAAh2kABAAAAAEAAABaAAAAAAAAAEgAAAABAAAASAAAAAEAA6ABAAMAAAABAAEAAKACAAQAAAABAAABAKADAAQAAAABAAABAAAAAACU0HdKAAAACXBIWXMAAAsTAAALEwEAmpwYAAABWWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgpMwidZAAAf1ElEQVR4Ae1dC4xcV3n+d3dmdva93vV6vbZxEucJcR6Ql0PSUpoWVaUKrVoUQEWlAhUhSlVK2whRtRRSCVBDK6VABW0RbYNKEdAqqA8KQhRoQmzHSRxs5+G3d73e92t2d2Zf/b8zc+3NZr3ZOffMnXvmfmc1O8977n++//+/859zz39u3YoWYSECRCCRCNQnstVsNBEgAgYBEgANgQgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGAESQIKVz6YTARIAbYAIJBgBEkCClc+mEwESAG2ACCQYARJAgpXPphMBEgBtgAgkGIFUrbV9eXk5dJPq6uoEjziUFRViZQX/7Yppi92hzo4y0ts3wZkcG1a0Rt1r3m54qM9f1qlxxV01VcEXsFSbBKAZFzzkqh4bRcC4fHYm4x3aAJ/bsJHeaioCyOfz8sMf/lAmJiakvr6+rJ4Tzo7ooa2tTfbt2ycdHR3m+GqRQOC0k3OLcuDEmEzNFGRF/4qmGDyvVW3pcz04la6X3s4muXFXh7Q2Nmhb3JDJ2jNu9D6Qcia/KGfG5mV+YWmjn7/iOxxfr3rJaluyKdXnK37h4AMFpqG+zjxSDfWS0Uejni/TUCdpfb2agKuBoYMWblhFTRBA0Ft/5zvfkfvvv3/DBm/my4985CPy0EMPSTabrQoJBIY2Pb8of/zNY/LFl6ZKfl+GC6hRq/fIu3a2yCd/+WrZs60l0rYEzj8xuyCPHRmR0dlFA30ZLTC/R88LJ9zRlpHObIMsAxzHRQd8Fx29CFuRdFoyDdKeTcmW5rSSaMqQBE4dtM2xGFWpriYIIEDu5MmT5uV9990nc3NzwcebfkZv39DQIA8//LB8+MMflp07d276WJc/RE8Pozx0aly++MKkvKU7I/Nl2j38JKWO89VzM5L9rxPy+XfdKI2lXhROVfFS8pLnh3IyrM7fqVGIjfNC1iWta25hWXqa1VxXd8kuG1HCt/Qks4UlmckvyeBUwTh+u5LPtrZG6VUiyqYbzJkDonYpRtR11RQBpNNpg18ul5PZ2dmysQQBpFIp6erqUjuLxE3WlTE48/B0QUTD0Vmd15xbDkxz3UMu++Gb29LyD2em5YGjw/KWm3ojiwIC+GYLy5LWBsH5LZtghi9LevCiPhCuR1Ew9NBRgIm84OhTGo2NK5GdGp2TXZ2NsruryQwRopClkueoqcuAwXxmWOednp6uJOabrnt+elxkoRCKjGbVaa7L1Mvf7x+Uae3RYNgw6ChLxKdz1jTIHWDVoLhlNKRa0g+Oj8zJgTOTMqLzMr6XmiIAV8oIiMRVfbb1LM3nRAYvSJ1OaF60xDIrW1Qr3q5j2X/tz8n/Hh0yRxcnE8usKOE/D8gAQQGIAEOEQ+em5PjwpUjTR6IjAcTYsOvqVD0vnpbFQr5IApayzmuvdXNTg/zV4+dlNFeoShRgKXosD0NUgIggrcORl0Zy8tPz0xcnBn0jARJALE2sJBQIYGxaZkYu6FhUXwfxaJkyYxKtQycAvzcyL989XIwCinPZZVbEn19EAI6OByZW+yfychQkoB9EM0NxUYzQL0gAoSGsZAVqUZmUTJ3vl8L8bOgo4BaNAv7mwKAMTWlEUYW5gEoiVa264fQYEpydmNe5AR2yaQEx+FJIAHHXlPYwi3pJc3RwoHgJzDIKwAx8m9b1o4mCPPb0YKnVPplqfBUFlTTqoqGTeoXAkKuKaqmmyBtJAogc8jJPqJ5br5cm+wfPS342p1GAXoO2tK681nW7RgGPHBqW/vH5UhRAEihTI5f9Oa5QvjQyK4WlZcX2sj+L1RckgFipY31hzGXNQkGGB84Vw0tL40KaVFZ7qmd0fcG/H9SIwhTLykpH86mIAGgUaxSweOjsWHERmg/USgLwwILNykCNAgYvDMrczGSoKGBeo4C7W1LyIY0CXtJVeuipbFboeQBb5CIiMEvp6qFzOimIy4Sg1riTAAkgcjOxOCGsCJ66tFSMAjCgt4wxURUWAy3rCr1vHihGAXjP4gYBDAPyi8syNJ0vVhhzBiABuNF7xWsxCU/plAwPDUluarx4RSDEXMDdzQ3y4OExOdqviUZafIkC4k5VUImOsuSCDrOwfDnu3EoCqLjrujxB0fwv9J81qcu21oVOycwr6GTVo08OCOYGfIgCzKVLlRVOdvGB9+U8gmP1GBQg6ppUgCVSoJE/gAL54lpIAHHVzDpymShA5wLGR8dkemwkdBRwr14R+IvnJ+Tw6QlztrhHAViHj4m2tMnVLz3jfTmP0rFYyYeR1IL+wwPFFRGg11duFaRCx73UVDZg3MF2Jp/S9sC5s9La2WU2PrGtd0kttVWd6h9/MiCfek2HcSS4gitHsJVr7XFwKDjp7i1Zuaq72fYq6MuqxcTqgiZKoKfG8uiR3IJJ9AExhO6xtQLIHEQAccNzNRAkgNVoePAaUUB9Q0pyk5MyNTokXb07ZXlJQ01YXJmloE71+qaUfPbFSfn142Pyxmu71bmqvxXa2mbAIZGa29feaJberv3e9n2TZo+3a/t3dGYFm68cGZwxzymNKMKQAI7VKmRWd0Ba0FAAOwvFtcRXsrgiFgO54KSYaTqnUUChoAt6kC1oWRa0rp26lPUrGgVg9hrj1zDGbynGxoepQPXwqFJB88MWVGEe+g/Pbbrzzy072wW7AGGoEbYAx3ndxKSgmKKErzGsROsfb28569fHTyNAAMZUrzsX5WdmZGI4XKLQgla2R3fr+eKpGXn8xVEjvSGYCNpR1ilWe9AlLiiritU/RhXmof/wjDZj78Eru5vM3AA+C1NwPDYwWUAmVowLCSDGytlINDNhpxOC5/rPaaLQXKgooKDGf3WmTr70xIDkdAGLiQLibbcbQWP1nbkqokdubc1Is0YBpXlBq7pWH4SoKs6FBBBn7byKbAj9l5AopNmCZg7AMnTFpiE71ei/ek43DTmS7E1DMF7v1HmBRcXSYlrlFRpDXkCcCwkgztp5FdlMqK77IPZrpuB8bkajAPtEIWwa8lrd+PILTw4KtiJPYhQQ8GczNv1MSAREAngVJ4v71yZ0XVjQdGGNAmC1loNXDFW36hj4saE5+e5zOq9gSkK8oNTaALuMpk2b3j9M81UPOHyRcwABunyuBAKIAup0LuD84KDMTk+FjgJu1ijgcxoFYMNLs/IujBNUosER1InFRpY8+nLpFDssB45zYQQQZ+1sVjZ0V5ooNIR0YRic5eAVh2LrsO+P5eU/krxpSLx9drNWsanfkQA2BVO8fxREASPDmig0GS5RCOnCt2ui0GcPDsmAbnNVjAIS5BHxVrVz6UgAziGtUoWlXv98/xldGaj34LONAlT8YNOQbx86X6XG8LRRIUACiArpCp8HUQC2Dpsc00Sh8VFdORfiioBGAXfqbbg++dSwnNItrhgFVFh5VayeBFBF8F2f2gTqujbgvKYLLy7qXWssowDUgwy7c/NL8o3SpiHmaoNrgVlf1REgAVRdBe4EMFGALhGe0dujT44Mh7oigA1E9+lcwEefHZUXNEkGJe7pwu6QTE5NJIAa0zVIQG9xLP2IAkLcUQhRAFJjF3Qp69d00xC8x+IgltpCgARQW/osOmopUWh8SPf/D3FHIVwRuEc3DfnTI2Py3NlJgxSjgNoyGBJAbenzkpMiUWgg/B2FVrTXz2hU8c+aLoycAUYBtWUwJIDa0ufF1tSbRKFZGbugl/IQumNoYFEwF3CnRgGf0U1Dnj6ltyvXwijAAsiYHkICiKliwoplnFQThRAFIFEok2qwXt66uFInvcohX35cIwpd224ShcIKyONjgQAJIBZqqIwQ5tKdJgqNaLrwGb0PAEjBZhoPuwZdpzkCnz8xLU+8OGKENZONlRGbtUaIAAkgQrCjPlUaU4Ir9bI7PyGfuadHN7pIWW93hU1DrkrXyd/95PylTUOibhDP5xwBEoBzSONToVGuLug5OLsob7qhW969t1sO5RalST8rt2AC8DW6ddg/nZ2RHx9jFFAufnH9PQkgrppxIJeZ9lNfR2qALgyWX3lDn2xvSQt2Ay6fAvSWVxoFXJ+pl7/VKABbXidx0xAHaolVFSSAWKnDrTAXnVx3/c3rDrU7Ohrl42/okSdDRAHbdOuwb12Yk++Xtg4rbnvhVm7WFh0CJIDosK7qmYKLgG/TKOCezoxMmy3AyxcJUcBN2Xp55InzMprgTUPKRy6eR5AA4qkX51IFw/7tHVn53dt75em5JclaLO3FDledumnI90bz8t/PJnTrMOfaqV6FJIDqYV+1M//iTb1y39asTGoUgDvulFuwgehtmij01wcuyOBksGlIubXw93FAgAQQBy1EKAP2qOvWve//4O4+edYyCtAqpFm3z94/VZBvP6X5BqYEg4wIG8NThUaABBAaQr8qCKL+n3ltj7x9R4sM6o1AdI6w7DKHJcK6acinnx6Ws2N6YxKtmIuDyoax6geQAKqugmgFgK9jRWBbY0ref1efvJBflkzACmWIgv4em4a8pGsM/u1gsHWYBZOUcU7+1D0CJAD3mMa+xmB3n3tv2Cof2NMuJ/JLoov8yi5IFLpLE4U+rlHA8aFcKeeIQ4GygaziASSAKoJfrVMHUUCjzua/+84+6dc7hNpGAbiV9piuMfja/uKmIQG5VKttPG95CJAAysOrZn4dOOrtV3fJH17XIQf1dmCZ4FphGa0MNg352OFROdI/ZY5kunAZAFb5pySAKiugWqcPogCM43/zrh2iAbwuF7YL37FpSJ3OK/yLbh2GKwRcIlwtrZZ/XhJA+ZjVzBHB7j43X9Epf35jl/wop4uDLKIAzAVg67CHjk7IodMTBp8VSzKpGXA9aQgJwBNFVUpMXLpDNPDAHX2S1ZuDLpXel3s+bBrSo9b0Fd00ZKG0aUi5dfD30SNAAoge81idsXj9XuT6vjb51C1b5QlNFLKJArBpyPW6acgjJ6bkwImxWLWRwlweARLA5bFJ0DfFsf/bbuuTvqaULGhIb3FVUDcNXZHduqroS48PyKxeGUDhUCDeZkQCiLd+IpGuGAWsyJVbm+UTt22Tn+jiHrsoQORK3TTky6en5fEXgk1DImkCT2KJAAnAErjaO6zY57/11u2yV3MF5peWraIApAtf21gvX3hiwGwdhrUGuDLAEk8ESADx1EvkUmE1MCYE+zqz8tG7euXA7JL11mG9umnIN87Pyg+ODOtNSm0GE5E3P7EnJAEkVvXrNbzorL90c6+8RdOFJ2zThbXLf4POJXziB2flscND0qxXFxgFrId39T8jAVRfB7GRAFEAVvF1tWTkg5oodFijAJtNQxDxa7awFJRAvvXCuEzMLuh7ZAvGpqkUpIQACYCm8DIEdE2fef8mTRf+jR3NMqSz+TbpwujxM3p3onr1+sP90yYCsEg6fJlsfOMeARKAe0y9rjGIAjo0hP/A3Tvk2PySNKL3tmgVLgu2aihwaHhOhnTnIEYBFiBW+BASQIUB9rH6IFFo33Xd8p4r2uS0kkDGdi4PwwoNB54+Ny2L+swoIF4WQQKIlz5iIQ18HXMBzekGed++HXJa7wqStvRcDAXadAzx7Ni8DIzPMQqIhYYvCUECuIQFX61CIEgUuuvaLvkjTRd+yjJdGFVi+JDRxyGNAjAxyCuDQCUehQQQDz3EUgpEASnt+d95h6YLa7KPbbowooBmjQKOTOTlzOisNOhWxNw/MB4qJwHEQw+xlCKIAm65slM+fuMW+ZFeFsSEoE0BCbTosU/1z8isbkTKBUI2KLo/hgTgHtOaqhFRAIzkHXfuKF7c1/c2BUdlted/abogxy/kSnMBdnXZnJ/HrI8ACWB9XPhpCYEgCkC68Kdv6pb/w+KgEFFAu5LAwYEZmdY5BXNZkEhXFQESQFXh9+PkwXj9AY0CdmvOv226MPp77Ds4oNmGL2oUYC43MgioqhGQAKoKvx8nD9KFr9B04Y/h7sKW6cJoLeYCOjRD8EmNAsZzMV0ibDfN4Ycy10hJAlgDCN9eDoGiV7z11j65vjUted32y8ZP0OGr/8u4TgQeOz8jVpVcTkRHn2PhkpPARAGK+2QnCcCR0dR6NVgHhKHAzi1Z+bO7tst+jQKaLOcCgjsMH9RhwPBUXlLmsmAMECx5PdYqmLlOG4YLmqF14fC0ti3OhQQQZ+3ETraiMf/C3l752a5GmQmxqAfcMafHFxOF4rFEOFjsOLew5AR5wydOQgkn4qxbCQlgXVj44XoIBFFAT1tGfl/ThQ9h05DAa9Y7YIPPEAW061jgmZE5GdQFQnFJFMLdkyfncYUCEc8GDdjEV6BLRDdxLiSAOGsnlrIVDfq+vdvkHbvs7y5smoaq1Mme1jsKYStxSy5xglLg7OO6d8GM3isxuPwZtvIsJjxiXOItXZWAa2hoKF6iqtL543xaOCkWB7VnU/Lbd2w3dxdutPRcXBFo1SXCh0fz0q+3GN/0XEDInnk1vqgKzo8moPc/qUuVUYo0Z15a/UO9iGrSJAAr/KpyECa56nUTi3w+L5OTk0aG5eVlTWeN/oGTw9FCW2IFkAzShe+9oUfed2Vb6e7Cdi4DR8lqN/SUbhoyr5uPXG5eEb8LzusSE0gN58e5nxuYlkldoIQbnuJ8YQp0h94/g7GEFjt0wkiwuWMZAazBCb0/yunTp80z3oMUonwEMmQymkOnqbiuwlHTIAf/YMwwcOz19x6dCzhn0oXtKkYU0KTj5Ocn83JyeNZcNgPvrS5B7zw6Uyh+rN/jN/hZ2Y/ScejtMdmHkP/4cE72n5mQYa3fhfMDH8jXpPhkYh4BpFYDzdfaE8zPy7333itf//rXzeve3l5VJswswqKnS6nhHD2qJNTdKDndojtuJeiN77i6Wz6454J8Te8FcK3eE6BggZUZCmive1DTha/obpLG0iaicCQUVInLacdHZmVInTT4vPht+f9xPG6BhhWNeb0SoQGecXwXzm+k0ROgTRgmocB6wspsKqrAPxIAlKNdDMJ89PJbt26VlpYW6e/vlwcffFBGRkYuhZ4VUMC6VdZnpG5mQPL7fkek50YZXlwoxqnr/rg6H8KgEQVk1DHfe/dO+dypY3KdZeAMB2nUes5ob4wlwjfvbpdldUwTm5eaFzjQTH7REELYViOqwP6HDfoipV4AkoEcTopWhKFMR1PaSXWVrCTxBADnX1paknQ6Ldu2bZPGxkbzHuH33r17DSlEHgGoYdbLgpyQNjmgxIRNORcraQWWdQdDk1s1XfhPbuiUh46Nyxt1L0HcLbjcYnpMHS/v1yXCV2xt0t4zbSbl4KhBwcvgcuGqj4Ovy34OpITzuyyILlr03gjYVxHFhawu5VtdV6IJIHB+OH1PT4/A6eHs+BzPhUJpzLkasUhea9+0kpeFNGLT8BNSlRQZUQCI4AHdNOSh5ydUbjtvwlEa+csFnYR7cTAnt1/V+Qqx8ZugeruzvKJK5x+AsLQ/EayVSCuhQc44E0BiJwED529qahKM8wPnd24RthXG2WpWtSmIAvbuape/vHWr/DhEujAWB21RFth/PidjmJDTYUFcHX0VBC97CYKC3L1tjS/7PK5vEkkAcP7FxUVpbW01YX9KB4HRh/lxNYny5UIUgPJrenfhrE4EIpnGlr/Ud2RKZ+efC+4lUL44VTsCvT8WNPWp87eVJgBtcYiqEYkkAIz5Ozs7zYQfLrnR+cOZG6IAcMCenhZ5+PU98niIdGFEAUgXfkrvJYBEoeKmIUWCCSdl5Y/GPEZWI5gr9UoGig9SJ44AMNu/ZcsW6e7uvjjWr7xpJOcMiAJu0/HvnF66tDUu9KS4h8AzukTY3EvAOp6IDncjs7LXHnX+Jp0ABCHGvfcHOrY6ig5Zh2dCT4/LfCAA9voOgdWq4ADAFHcX/tBt2+RgiLkAc0VAJz+fGZnXRCG9o1DM5wLQ9oIuhtrZ2Si7thR7f3zmQ6l5Aghm9KEMzPS3t7fT+StmmUWrx92Ff747K1N6LR9jepuC8BmXqHBHIeTnW1Zjc+qyjoGjY9zf3ZKW63tbzbE+hP5BI2uaAOD8wQIfzPRj0o89f6B6989wBkwI9rY3yu/ppiHPzIXbQLRFo4Cfjl+6l4B7icPViPai59+iC35u3tl+cRlxXMlqvdbWLAEEl/kww799+3bB5T46/3om4Paz4O7CP/e6bXL/dr27cCFEFKBdaZNaKO4lgGQd6DQOJRADzr+jo1Fu0UugWKrsy7h/NYY1SwCY6ccCHzg/npPs/FG6DZwDWJu7C2ui0LH8slinC6ulIlHo2FRBTg7lTKLQauON+nXg+MHeBTf0tsjeHW1F51dhgu+jlivM+WqSABD2Nzc3mwU+WOLrt/PbjyiDI9uiZABYY8kT3vy6Hvmt17TIgG6wYX1HIa2uXRfWH9EcgXldH4A19kG7whj+Zo9FUwAfenc4PoY4fTrEuWN3h+zuunS5L2qINyv/q/2uppYCo9dH6ejoMCG/r9f4sYymYWVJWhqWpSnVIJ1qdGt3qdtMOIxNO09pItF1LRnZ0lo01qI5v5pZhPu+6DAr0qjX899/zy75ylePyVZ9jc0/Fsr0XkwiLtRp0pG2Bdl6KPhffGXeuvm3ukKVEfmX6DiQiImvkNvf15qRvo6sdJaSfEAKAUG4ESL6WmqKAK655hqDIFb5IYsPkcBmHCV62Dc+oyEATf/J1S3KXFOnzNXBeWGSgZWq5WHFzKsV/PzgrLz3y2+UtubiMCgqPILz3H1Nl/znr+6Rd/7PGZmY0qxGm8sCOtZ++42tZoltXl9XKtSGQ6NoUqjmJdRpWnJKk5IapKs5LZ36yOrt0oOCn1ZKjuAcUTxr7kbQ7ChOV5lzoAkwuFwuJ48++qhJ5fW197+EEKbTlqWgzj9Tv0Wfsxr6qkdrW+t19WK2tb1kgQERBORQpAnsbN+qTr/vtbvkvjuu140pSotTLv3s0qkieIW7Ah84OSEnNKe/OKG3cSgPMWGZzbqo5irNDrx6e+tF+nMtLpJ2EFzgGVELHlk9L3p9rERcXYzjr/7A89c1QQBrdeBrz7+2HcX3wU0qLiXGwCSDHnb9Y4qfrjbdahpuQNAbyRrn70BEtdDbr4dxTRGA74a2noLCfmYCPLXe1WQQtk6b4+FE6PM3Q1xr64/KAUsimhCq2nitxaBS72uKACoFEuslArWKQE1eBqxVZbFdRMA1AiQA14iyPiLgEQIkAI+URVGJgGsESACuEWV9RMAjBEgAHimLohIB1wiQAFwjyvqIgEcIkAA8UhZFJQKuESABuEaU9REBjxAgAXikLIpKBFwjQAJwjSjrIwIeIUAC8EhZFJUIuEaABOAaUdZHBDxCgATgkbIoKhFwjQAJwDWirI8IeIQACcAjZVFUIuAaARKAa0RZHxHwCAESgEfKoqhEwDUCJADXiLI+IuARAiQAj5RFUYmAawRIAK4RZX1EwCMESAAeKYuiEgHXCJAAXCPK+oiARwiQADxSFkUlAq4RIAG4RpT1EQGPECABeKQsikoEXCNAAnCNKOsjAh4hQALwSFkUlQi4RoAE4BpR1kcEPEKABOCRsigqEXCNAAnANaKsjwh4hAAJwCNlUVQi4BoBEoBrRFkfEfAIARKAR8qiqETANQIkANeIsj4i4BECJACPlEVRiYBrBEgArhFlfUTAIwRIAB4pi6ISAdcIkABcI8r6iIBHCJAAPFIWRSUCrhEgAbhGlPURAY8QIAF4pCyKSgRcI0ACcI0o6yMCHiFAAvBIWRSVCLhGgATgGlHWRwQ8QoAE4JGyKCoRcI0ACcA1oqyPCHiEAAnAI2VRVCLgGgESgGtEWR8R8AgBEoBHyqKoRMA1AiQA14iyPiLgEQIkAI+URVGJgGsESACuEWV9RMAjBEgAHimLohIB1wiQAFwjyvqIgEcIkAA8UhZFJQKuESABuEaU9REBjxAgAXikLIpKBFwjQAJwjSjrIwIeIUAC8EhZFJUIuEaABOAaUdZHBDxCgATgkbIoKhFwjQAJwDWirI8IeIQACcAjZVFUIuAaARKAa0RZHxHwCAESgEfKoqhEwDUCJADXiLI+IuARAiQAj5RFUYmAawRIAK4RZX1EwCMESAAeKYuiEgHXCJAAXCPK+oiARwiQADxSFkUlAq4RIAG4RpT1EQGPECABeKQsikoEXCPw/wMnrnSYEqYJAAAAAElFTkSuQmCC',
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
          bitcoind: {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
          'btc-rpc-proxy': {
            title: 'Bitcoin Proxy',
            icon: 'assets/img/service-icons/btc-rpc-proxy.png',
          },
        },
      },
      latest: {
        icon: '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',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestLnd,
        categories: ['bitcoin', 'lightning', 'cryptocurrency'],
        versions: ['0.11.0', '0.11.1'],
        'dependency-metadata': {
          bitcoind: {
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
      latest: {
        icon: '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',
        license: 'licenseUrl',
        instructions: 'instructionsUrl',
        manifest: Mock.MockManifestBitcoinProxy,
        categories: ['bitcoin'],
        versions: ['0.2.2'],
        'dependency-metadata': {
          bitcoind: {
            title: 'Bitcoin Core',
            icon: 'assets/img/service-icons/bitcoind.png',
          },
        },
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
      message: new Array(50)
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
      timestamp: '2019-12-26T14:20:30.872Z',
      message: '****** START *****',
    },
    {
      timestamp: '2019-12-26T14:21:30.872Z',
      message:
        '2021/11/09 22:55:04 \u001b[0;32;49mPOST \u001b[0;32;49m200\u001b[0m photoview.embassy/api/graphql \u001b[0;36;49m1.169406ms\u001b[0m unauthenticated',
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
    ],
  }

  export const BackupTargets: RR.GetBackupTargetsRes = {
    hsbdjhasbasda: {
      type: 'cifs',
      hostname: 'smb://192.169.10.0',
      path: '/Desktop/embassy-backups',
      username: 'TestUser',
      mountable: false,
      'embassy-os': {
        version: '0.3.0',
        full: true,
        'password-hash':
          '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNK',
        'wrapped-key': '',
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
    //   'embassy-os': null,
    // },
    csgashbdjkasnd: {
      type: 'cifs',
      hostname: 'smb://192.169.10.0',
      path: '/Desktop/embassy-backups-2',
      username: 'TestUser',
      mountable: true,
      'embassy-os': null,
    },
    // 'powjefhjbnwhdva': {
    //   type: 'disk',
    //   logicalname: 'sdba1',
    //   label: 'Another Drive',
    //   capacity: 2000000000000,
    //   used: 100000000000,
    //   model: null,
    //   vendor: 'SSK',
    //   'embassy-os': {
    //     version: '0.3.0',
    //     full: true,
    //     'password-hash': '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    //     'wrapped-key': '',
    //   },
    // },
  }

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
      Test: {
        type: 'string',
        description: 'This is some information about the thing.',
        copyable: true,
        qr: true,
        masked: false,
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
  } as any // @TODO why is this necessary?

  const realWorldConfigSpec: RR.GetPackageConfigRes = {
    config: {
      bitcoind: {
        'bitcoind-rpc': {
          type: 'internal',
          'rpc-user': 'btcpayserver',
          'rpc-password': '3ZRIgPq60y3bpWRPXKPdhV',
        },
        'bitcoind-p2p': {
          type: 'internal',
        },
      },
      lightning: {
        type: 'lnd',
      },
      'tor-address':
        'fzo2uwpfx3yxgvb2fjhmkbxa27logah3c7zw3fc6rxq5osqyegyy34qd.onion',
    },
    spec: {
      'tor-address': {
        type: 'pointer',
        subtype: 'package',
        target: 'tor-address',
        'package-id': 'btcpayserver',
        interface: 'network',
        description: 'The Tor address for the network interface.',
        name: 'Network Tor Address',
      },
      bitcoind: {
        type: 'object',
        spec: {
          'bitcoind-rpc': {
            type: 'union',
            tag: {
              id: 'type',
              name: 'Type',
              description:
                'The Bitcoin Core node to connect to:\n  - internal: The Bitcoin Core and Proxy services installed to your Embassy\n  - external: An unpruned Bitcoin Core node running on a different device\n',
              'variant-names': {
                external: 'External',
                internal: 'Internal',
              },
            },
            variants: {
              external: {
                'connection-settings': {
                  type: 'union',
                  tag: {
                    id: 'type',
                    name: 'Type',
                    description:
                      '- Manual: Raw information for finding a Bitcoin Core node\n- Quick Connect: A Quick Connect URL for a Bitcoin Core node\n',
                    'variant-names': {
                      manual: 'Manual',
                      'quick-connect': 'Quick Connect',
                    },
                  },
                  variants: {
                    manual: {
                      'rpc-host': {
                        type: 'string',
                        copyable: false,
                        masked: false,
                        nullable: false,
                        default: null,
                        description:
                          'The public address of your Bitcoin Core server',
                        name: 'Public Address',
                      },
                      'rpc-user': {
                        type: 'string',
                        copyable: false,
                        masked: false,
                        nullable: false,
                        default: null,
                        description:
                          'The username for the RPC user on your Bitcoin Core RPC server',
                        name: 'RPC Username',
                      },
                      'rpc-password': {
                        type: 'string',
                        copyable: false,
                        masked: false,
                        nullable: false,
                        default: null,
                        description:
                          'The password for the RPC user on your Bitcoin Core RPC server',
                        name: 'RPC Password',
                      },
                      'rpc-port': {
                        type: 'number',
                        range: '[0,65535]',
                        integral: true,
                        nullable: false,
                        default: 8332,
                        description:
                          'The port that your Bitcoin Core RPC server is bound to',
                        name: 'RPC Port',
                      },
                    },
                    'quick-connect': {
                      'quick-connect-url': {
                        type: 'string',
                        copyable: false,
                        masked: false,
                        nullable: false,
                        default: null,
                        description:
                          'The Quick Connect URL for your Bitcoin Core RPC server\nNOTE: LND will not accept a .onion url for this option\n',
                        name: 'Quick Connect URL',
                      },
                    },
                  },
                  'display-as': null,
                  'unique-by': null,
                  default: 'quick-connect',
                  description:
                    'Information to connect to an external unpruned Bitcoin Core node',
                  name: 'Connection Settings',
                },
              },
              internal: {
                'rpc-user': {
                  type: 'pointer',
                  subtype: 'package',
                  target: 'config',
                  'package-id': 'btc-rpc-proxy',
                  selector: '$.users.[?(@.name == "btcpayserver")].name',
                  multi: false,
                  description:
                    'The username for the RPC user allocated to BTCPay',
                  name: 'RPC Username',
                  interface: 'asdf',
                },
                'rpc-password': {
                  type: 'pointer',
                  subtype: 'package',
                  target: 'config',
                  'package-id': 'btc-rpc-proxy',
                  selector: '$.users.[?(@.name == "btcpayserver")].password',
                  multi: false,
                  description:
                    'The password for the RPC user allocated to BTCPay',
                  name: 'RPC Password',
                  interface: 'asdf',
                },
              },
            },
            'display-as': null,
            'unique-by': null,
            default: 'internal',
            description:
              'The Bitcoin Core node to connect to over the RPC interface',
            name: 'Bitcoin Core RPC',
          },
          pruning: {
            default: 'disabled',
            description:
              'Blockchain Pruning Options\nReduce the blockchain size on disk\n',
            'display-as': null,
            name: 'Pruning Settings',
            tag: {
              description:
                '- Disabled: Disable pruning\n- Automatic: Limit blockchain size on disk to a certain number of megabytes\n- Manual: Prune blockchain with the "pruneblockchain" RPC\n',
              id: 'mode',
              name: 'Pruning Mode',
              'variant-names': {
                automatic: 'Automatic',
                disabled: 'Disabled',
                manual: 'Manual',
              },
            },
            type: 'union',
            'unique-by': null,
            variants: {
              automatic: {
                size: {
                  default: 550,
                  description: 'Limit of blockchain size on disk.',
                  integral: true,
                  name: 'Max Chain Size',
                  nullable: false,
                  range: '[550,1000000)',
                  type: 'number',
                  units: 'MiB',
                },
              },
              disabled: {},
              manual: {
                size: {
                  default: 65536,
                  description: 'Prune blockchain if size expands beyond this.',
                  integral: true,
                  name: 'Failsafe Chain Size',
                  nullable: false,
                  range: '[550,1000000)',
                  type: 'number',
                  units: 'MiB',
                },
              },
            },
          },
          'bitcoind-p2p': {
            type: 'union',
            tag: {
              id: 'type',
              name: 'type',
              description: null,
              'variant-names': {
                external: 'external',
                internal: 'internal',
              },
            },
            variants: {
              external: {
                'p2p-host': {
                  type: 'string',
                  copyable: false,
                  masked: false,
                  nullable: false,
                  default: null,
                  description: 'The public address of your Bitcoin Core server',
                  name: 'Public Address',
                },
                'p2p-port': {
                  type: 'number',
                  range: '[0,65535]',
                  integral: true,
                  nullable: false,
                  default: 8333,
                  description:
                    'The port that your Bitcoin Core P2P server is bound to',
                  name: 'P2P Port',
                },
              },
              internal: {},
            },
            'display-as': null,
            'unique-by': null,
            default: 'internal',
            description:
              'The Bitcoin Core node to connect to over the P2P interface',
            name: 'Bitcoin Core P2P',
          },
        },
        'display-as': null,
        'unique-by': null,
        description:
          'RPC and P2P interface configuration options for Bitcoin Core',
        name: 'Bitcoin Settings',
      },
      lightning: {
        type: 'union',
        tag: {
          id: 'type',
          name: 'Type',
          description:
            'Enables BTCPay to use the selected internal lightning node.',
          'variant-names': {
            'c-lightning': 'C-Lightning',
            lnd: 'LND',
            none: 'No selection',
          },
        },
        variants: {
          'c-lightning': {},
          lnd: {},
          none: {},
        },
        'display-as': null,
        'unique-by': null,
        default: 'none',
        description:
          'Use this setting to grant BTCPay access to your Embassy\\\'s LND or c-lightning node. If you prefer to use an external Lightning node, or you do not intend to use Lightning, leave this setting blank. Please see the "Instructions" page for more details.',
        name: 'Embassy Lightning Node',
      },
    },
  }

  const testSpec: ConfigSpec = {
    testnet: {
      name: 'Testnet',
      type: 'boolean',
      description:
        'determines whether your node is running on testnet or mainnet',
      warning: 'Chain will have to resync!',
      default: true,
    },
    'object-list': {
      name: 'Object List',
      type: 'list',
      subtype: 'object',
      description: 'This is a list of objects, like users or something',
      range: '[0,4]',
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
      // it just so happens that ValueSpecObject's have the field { spec: ConfigSpec }
      // see 'union-list' below for a different example.
      spec: {
        'unique-by': 'last-name',
        'display-as': `I'm {{last-name}}, {{first-name}} {{last-name}}`,
        spec: {
          'first-name': {
            name: 'First Name',
            type: 'string',
            description: 'User first name',
            nullable: true,
            default: null,
            masked: false,
            copyable: false,
          },
          'last-name': {
            name: 'Last Name',
            type: 'string',
            description: 'User first name',
            nullable: true,
            default: {
              charset: 'a-g,2-9',
              len: 12,
            },
            pattern: '^[a-zA-Z]+$',
            'pattern-description': 'must contain only letters.',
            masked: false,
            copyable: true,
          },
          age: {
            name: 'Age',
            type: 'number',
            description: 'The age of the user',
            nullable: true,
            default: null,
            integral: false,
            warning: 'User must be at least 18.',
            range: '[18,*)',
          },
        },
      },
    },
    'union-list': {
      name: 'Union List',
      type: 'list',
      subtype: 'union',
      description: 'This is a sample list of unions',
      warning: 'If you change this, things may work.',
      // a list of union selections. e.g. 'summer', 'winter',...
      default: ['summer'],
      range: '[0, 2]',
      spec: {
        tag: {
          id: 'preference',
          name: 'Preferences',
          'variant-names': {
            summer: 'Summer',
            winter: 'Winter',
            other: 'Other',
          },
        },
        // this default is used to make a union selection when a new list element is first created
        default: 'summer',
        variants: {
          summer: {
            'favorite-tree': {
              name: 'Favorite Tree',
              type: 'string',
              nullable: false,
              description: 'What is your favorite tree?',
              default: 'Maple',
              masked: false,
              copyable: false,
            },
            'favorite-flower': {
              name: 'Favorite Flower',
              type: 'enum',
              description: 'Select your favorite flower',
              'value-names': {
                none: 'Hate Flowers',
                red: 'Red',
                blue: 'Blue',
                purple: 'Purple',
              },
              values: ['none', 'red', 'blue', 'purple'],
              default: 'none',
            },
          },
          winter: {
            'like-snow': {
              name: 'Like Snow?',
              type: 'boolean',
              description: 'Do you like snow or not?',
              default: true,
            },
          },
        },
        'unique-by': 'preference',
      },
    },
    'random-enum': {
      name: 'Random Enum',
      type: 'enum',
      'value-names': {
        null: 'Null',
        option1: 'One 1',
        option2: 'Two 2',
        option3: 'Three 3',
      },
      default: 'null',
      description: 'This is not even real.',
      warning: 'Be careful changing this!',
      values: ['null', 'option1', 'option2', 'option3'],
    },
    'favorite-number': {
      name: 'Favorite Number',
      type: 'number',
      integral: false,
      description: 'Your favorite number of all time',
      warning:
        'Once you set this number, it can never be changed without severe consequences.',
      nullable: true,
      default: 7,
      range: '(-100,100]',
      units: 'BTC',
    },
    'unlucky-numbers': {
      name: 'Unlucky Numbers',
      type: 'list',
      subtype: 'number',
      description: 'Numbers that you like but are not your top favorite.',
      spec: {
        integral: false,
        range: '[-100,200)',
      },
      range: '[0,10]',
      default: [2, 3],
    },
    rpcsettings: {
      name: 'RPC Settings',
      type: 'object',
      'unique-by': null,
      description: 'rpc username and password',
      warning: 'Adding RPC users gives them special permissions on your node.',
      spec: {
        laws: {
          name: 'Laws',
          type: 'object',
          'unique-by': 'law1',
          description: 'the law of the realm',
          spec: {
            law1: {
              name: 'First Law',
              type: 'string',
              description: 'the first law',
              nullable: true,
              masked: false,
              copyable: true,
            },
            law2: {
              name: 'Second Law',
              type: 'string',
              description: 'the second law',
              nullable: true,
              masked: false,
              copyable: true,
            },
          },
        },
        rulemakers: {
          name: 'Rule Makers',
          type: 'list',
          subtype: 'object',
          description: 'the people who make the rules',
          range: '[0,2]',
          default: [],
          spec: {
            'unique-by': null,
            spec: {
              rulemakername: {
                name: 'Rulemaker Name',
                type: 'string',
                description: 'the name of the rule maker',
                nullable: false,
                default: {
                  charset: 'a-g,2-9',
                  len: 12,
                },
                masked: false,
                copyable: false,
              },
              rulemakerip: {
                name: 'Rulemaker IP',
                type: 'string',
                description: 'the ip of the rule maker',
                nullable: false,
                default: '192.168.1.0',
                pattern:
                  '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                'pattern-description': 'may only contain numbers and periods',
                masked: false,
                copyable: true,
              },
            },
          },
        },
        rpcuser: {
          name: 'RPC Username',
          type: 'string',
          description: 'rpc username',
          nullable: false,
          default: 'defaultrpcusername',
          pattern: '^[a-zA-Z]+$',
          'pattern-description': 'must contain only letters.',
          masked: false,
          copyable: true,
        },
        rpcpass: {
          name: 'RPC User Password',
          type: 'string',
          description: 'rpc password',
          nullable: false,
          default: {
            charset: 'a-z,A-Z,2-9',
            len: 20,
          },
          masked: true,
          copyable: true,
        },
      },
    },
    'bitcoin-node': {
      name: 'Bitcoin Node Settings',
      type: 'union',
      'unique-by': null,
      description: 'The node settings',
      default: 'internal',
      warning: 'Careful changing this',
      tag: {
        id: 'type',
        name: 'Type',
        'variant-names': {
          internal: 'Internal',
          external: 'External',
        },
      },
      variants: {
        internal: {
          'lan-address': {
            name: 'LAN Address',
            type: 'pointer',
            subtype: 'package',
            target: 'lan-address',
            'package-id': 'bitcoind',
            description: 'the lan address',
            interface: 'asdf',
          },
        },
        external: {
          'public-domain': {
            name: 'Public Domain',
            type: 'string',
            description: 'the public address of the node',
            nullable: false,
            default: 'bitcoinnode.com',
            pattern: '.*',
            'pattern-description': 'anything',
            masked: false,
            copyable: true,
          },
        },
      },
    },
    port: {
      name: 'Port',
      type: 'number',
      integral: true,
      description:
        'the default port for your Bitcoin node. default: 8333, testnet: 18333, regtest: 18444',
      nullable: false,
      default: 8333,
      range: '(0, 9998]',
    },
    'favorite-slogan': {
      name: 'Favorite Slogan',
      type: 'string',
      description:
        'You most favorite slogan in the whole world, used for paying you.',
      nullable: true,
      masked: true,
      copyable: true,
    },
    rpcallowip: {
      name: 'RPC Allowed IPs',
      type: 'list',
      subtype: 'string',
      description:
        'external ip addresses that are authorized to access your Bitcoin node',
      warning:
        'Any IP you allow here will have RPC access to your Bitcoin node.',
      range: '[1,10]',
      default: ['192.168.1.1'],
      spec: {
        masked: false,
        copyable: false,
        pattern:
          '((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))',
        'pattern-description': 'must be a valid ipv4, ipv6, or domain name',
      },
    },
    rpcauth: {
      name: 'RPC Auth',
      type: 'list',
      subtype: 'string',
      description: 'api keys that are authorized to access your Bitcoin node.',
      range: '[0,*)',
      default: [],
      spec: {
        masked: false,
        copyable: false,
      },
    },
    advanced: {
      name: 'Advanced',
      type: 'object',
      'unique-by': null,
      description: 'Advanced settings',
      spec: {
        notifications: {
          name: 'Notification Preferences',
          type: 'list',
          subtype: 'enum',
          description: 'how you want to be notified',
          range: '[1,3]',
          default: ['email'],
          spec: {
            'value-names': {
              email: 'EEEEmail',
              text: 'Texxxt',
              call: 'Ccccall',
              push: 'PuuuusH',
              webhook: 'WebHooookkeee',
            },
            values: ['email', 'text', 'call', 'push', 'webhook'],
          },
        },
        rpcsettings: {
          name: 'RPC Settings',
          type: 'object',
          'unique-by': null,
          description: 'rpc username and password',
          warning:
            'Adding RPC users gives them special permissions on your node.',
          spec: {
            laws: {
              name: 'Laws',
              type: 'object',
              'unique-by': 'law1',
              description: 'the law of the realm',
              spec: {
                law1: {
                  name: 'First Law',
                  type: 'string',
                  description: 'the first law',
                  nullable: true,
                  masked: false,
                  copyable: true,
                },
                law2: {
                  name: 'Second Law',
                  type: 'string',
                  description: 'the second law',
                  nullable: true,
                  masked: false,
                  copyable: true,
                },
              },
            },
            rulemakers: {
              name: 'Rule Makers',
              type: 'list',
              subtype: 'object',
              description: 'the people who make the rules',
              range: '[0,2]',
              default: [],
              spec: {
                'unique-by': null,
                spec: {
                  rulemakername: {
                    name: 'Rulemaker Name',
                    type: 'string',
                    description: 'the name of the rule maker',
                    nullable: false,
                    default: {
                      charset: 'a-g,2-9',
                      len: 12,
                    },
                    masked: false,
                    copyable: false,
                  },
                  rulemakerip: {
                    name: 'Rulemaker IP',
                    type: 'string',
                    description: 'the ip of the rule maker',
                    nullable: false,
                    default: '192.168.1.0',
                    pattern:
                      '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$',
                    'pattern-description':
                      'may only contain numbers and periods',
                    masked: false,
                    copyable: true,
                  },
                },
              },
            },
            rpcuser: {
              name: 'RPC Username',
              type: 'string',
              description: 'rpc username',
              nullable: false,
              default: 'defaultrpcusername',
              pattern: '^[a-zA-Z]+$',
              'pattern-description': 'must contain only letters.',
              masked: false,
              copyable: true,
            },
            rpcpass: {
              name: 'RPC User Password',
              type: 'string',
              description: 'rpc password',
              nullable: false,
              default: {
                charset: 'a-z,A-Z,2-9',
                len: 20,
              },
              masked: true,
              copyable: true,
            },
          },
        },
      },
    },
  }

  export const ConfigSpec: RR.GetPackageConfigRes['spec'] = testSpec
  export const MockConfig = {}

  // export const ConfigSpec: RR.GetPackageConfigRes['spec'] = realWorldConfigSpec.spec
  // export const MockConfig = realWorldConfigSpec.config

  export const MockDependencyConfig = {
    testnet: true,
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
      license: '/public/package-data/bitcoind/0.20.0/LICENSE.md',
      icon: '/assets/img/service-icons/bitcoind.png',
      instructions: '/public/package-data/bitcoind/0.20.0/INSTRUCTIONS.md',
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
          health: {},
        },
        'dependency-errors': {},
      },
      'interface-addresses': {
        rpc: {
          'tor-address': 'bitcoinproxy-rpc-address.onion',
          'lan-address': 'bitcoinproxy-rpc-address.local',
        },
      },
      'system-pointers': [],
      'current-dependents': {
        lnd: {
          pointers: [],
          'health-checks': [],
        },
      },
      'current-dependencies': {},
      'dependency-info': {},
      'marketplace-url': 'marketplace-url.com',
      'developer-key': 'developer-key',
    },
    'install-progress': undefined,
  }

  export const bitcoinProxy: PackageDataEntry = {
    state: PackageState.Installed,
    'static-files': {
      license: '/public/package-data/btc-rpc-proxy/0.20.0/LICENSE.md',
      icon: '/assets/img/service-icons/btc-rpc-proxy.png',
      instructions: '/public/package-data/btc-rpc-proxy/0.20.0/INSTRUCTIONS.md',
    },
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
      manifest: MockManifestBitcoinProxy,
      'interface-addresses': {
        rpc: {
          'tor-address': 'bitcoinproxy-rpc-address.onion',
          'lan-address': 'bitcoinproxy-rpc-address.local',
        },
      },
      'system-pointers': [],
      'current-dependents': {
        lnd: {
          pointers: [],
          'health-checks': [],
        },
      },
      'current-dependencies': {
        bitcoind: {
          pointers: [],
          'health-checks': [],
        },
      },
      'dependency-info': {
        lnd: {
          manifest: Mock.MockManifestLnd,
          icon: 'assets/img/service-icons/lnd.png',
        },
        bitcoind: {
          manifest: Mock.MockManifestBitcoind,
          icon: 'assets/img/service-icons/bitcoind.png',
        },
      },
      'marketplace-url': 'marketplace-url.com',
      'developer-key': 'developer-key',
    },
    'install-progress': undefined,
  }

  export const lnd: PackageDataEntry = {
    state: PackageState.Installed,
    'static-files': {
      license: '/public/package-data/lnd/0.20.0/LICENSE.md',
      icon: '/assets/img/service-icons/lnd.png',
      instructions: '/public/package-data/lnd/0.20.0/INSTRUCTIONS.md',
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
      'current-dependents': {},
      'current-dependencies': {
        bitcoind: {
          pointers: [],
          'health-checks': [],
        },
        'btc-rpc-proxy': {
          pointers: [],
          'health-checks': [],
        },
      },
      'dependency-info': {
        bitcoind: {
          manifest: Mock.MockManifestBitcoind,
          icon: 'assets/img/service-icons/bitcoind.png',
        },
        'btc-rpc-proxy': {
          manifest: Mock.MockManifestBitcoinProxy,
          icon: 'assets/img/service-icons/btc-rpc-proxy.png',
        },
      },
      'marketplace-url': 'marketplace-url.com',
      'developer-key': 'developer-key',
    },
    'install-progress': undefined,
  }

  export const LocalPkgs: { [key: string]: PackageDataEntry } = {
    bitcoind: bitcoind,
    'btc-rpc-proxy': bitcoinProxy,
    lnd: lnd,
  }
}
