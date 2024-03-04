import {
  DataModel,
  DockerIoFormat,
  HealthResult,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'
import { BUILT_IN_WIDGETS } from '../../pages/widgets/built-in/widgets'

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    'ack-welcome': '1.0.0',
    theme: 'Dark',
    widgets: BUILT_IN_WIDGETS.filter(
      ({ id }) =>
        id === 'favorites' ||
        id === 'health' ||
        id === 'network' ||
        id === 'metrics',
    ),
    marketplace: {
      'selected-url': 'https://registry.start9.com/',
      'known-hosts': {
        'https://registry.start9.com/': {
          name: 'Start9 Registry',
        },
        'https://community-registry.start9.com/': {},
        'https://beta-registry.start9.com/': {
          name: 'Dark9',
        },
      },
    },
    dev: {},
    gaming: {
      snake: {
        'high-score': 0,
      },
    },
    'ack-instructions': {},
  },
  'server-info': {
    id: 'abcdefgh',
    version: '0.3.5.1',
    'last-backup': new Date(new Date().valueOf() - 604800001).toISOString(),
    'lan-address': 'https://adjective-noun.local',
    'tor-address': 'https://myveryownspecialtoraddress.onion',
    'ip-info': {
      eth0: {
        ipv4: '10.0.0.1',
        ipv6: null,
      },
      wlan0: {
        ipv4: '10.0.90.12',
        ipv6: 'FE80:CD00:0000:0CDE:1257:0000:211E:729CD',
      },
    },
    'last-wifi-region': null,
    'unread-notification-count': 4,
    // password is asdfasdf
    'password-hash':
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    'eos-version-compat': '>=0.3.0 <=0.3.0.1',
    'status-info': {
      'backup-progress': null,
      updated: false,
      'update-progress': null,
      restarting: false,
      'shutting-down': false,
    },
    hostname: 'random-words',
    pubkey: 'npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m',
    'ca-fingerprint': 'SHA-256: 63 2B 11 99 44 40 17 DF 37 FC C3 DF 0F 3D 15',
    'ntp-synced': false,
    platform: 'x86_64-nonfree',
  },
  'package-data': {
    bitcoind: {
      state: PackageState.Installed,
      'static-files': {
        license: '/public/package-data/bitcoind/0.20.0/LICENSE.md',
        icon: '/assets/img/service-icons/bitcoind.svg',
        instructions: '/public/package-data/bitcoind/0.20.0/INSTRUCTIONS.md',
      },
      manifest: {
        id: 'bitcoind',
        title: 'Bitcoin Core',
        version: '0.20.0',
        'git-hash': 'abcdefgh',
        description: {
          short: 'A Bitcoin full node by Bitcoin Core.',
          long: 'Bitcoin is a decentralized consensus protocol and settlement network.',
        },
        'release-notes': 'Taproot, Schnorr, and more.',
        assets: {
          icon: 'icon.png',
          license: 'LICENSE.md',
          instructions: 'INSTRUCTIONS.md',
          docker_images: 'image.tar',
          assets: './assets',
          scripts: './scripts',
        },
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
        'health-checks': {
          'chain-state': {
            name: 'Chain State',
          },
          'ephemeral-health-check': {
            name: 'Ephemeral Health Check',
          },
          'p2p-interface': {
            name: 'P2P Interface',
            'success-message': 'the health check ran succesfully',
          },
          'rpc-interface': {
            name: 'RPC Interface',
          },
          'unnecessary-health-check': {
            name: 'Unneccessary Health Check',
          },
        } as any,
        config: {
          get: {},
          set: {},
        } as any,
        volumes: {},
        'min-os-version': '0.2.12',
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
            description:
              'Use this to resync the Bitcoin blockchain from genesis',
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
                description:
                  'Your reason for re-syncing. Why are you doing this?',
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
                range: '[-1000, 1000]',
                integral: false,
                units: 'm/s',
              },
              testnet: {
                name: 'Testnet',
                type: 'boolean',
                description:
                  '<ul><li>determines whether your node is running on testnet or mainnet</li></ul><script src="fake"></script>',
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
                description: 'The person to contact in case of emergency.',
                spec: {
                  name: {
                    type: 'string',
                    name: 'Name',
                    nullable: false,
                    masked: false,
                    copyable: false,
                    pattern: '^[a-zA-Z]+$',
                    'pattern-description': 'Must contain only letters.',
                  },
                  email: {
                    type: 'string',
                    name: 'Email',
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
                type: 'union',
                default: 'internal',
                tag: {
                  id: 'type',
                  'variant-names': {
                    internal: 'Internal',
                    external: 'External',
                  },
                  name: 'Bitcoin Node Settings',
                  description: 'The node settings',
                  warning: 'Careful changing this',
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
                      interface: '',
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
        dependencies: {},
      },
      installed: {
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0',
        },
        'last-backup': null,
        status: {
          configured: true,
          main: {
            status: PackageMainStatus.Running,
            started: '2021-06-14T20:49:17.774Z',
            health: {
              'ephemeral-health-check': {
                result: HealthResult.Starting,
              },
              'chain-state': {
                result: HealthResult.Loading,
                message: 'Bitcoin is syncing from genesis',
              },
              'p2p-interface': {
                result: HealthResult.Success,
              },
              'rpc-interface': {
                result: HealthResult.Failure,
                error: 'RPC interface unreachable.',
              },
              'unnecessary-health-check': {
                result: HealthResult.Disabled,
              },
            },
          },
          'dependency-config-errors': {},
        },
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
                  preferredExternalPort: 443,
                  scheme: 'https',
                },
                secure: false,
                ssl: false,
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
                  preferredExternalPort: 443,
                  scheme: 'https',
                },
                secure: false,
                ssl: false,
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
                secure: true,
                ssl: false,
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
        'system-pointers': [],
        'current-dependents': {
          lnd: {
            pointers: [],
            'health-checks': [],
          },
        },
        'current-dependencies': {},
        'dependency-info': {},
        'marketplace-url': 'https://registry.start9.com/',
        'developer-key': 'developer-key',
      },
    },
    lnd: {
      state: PackageState.Installed,
      'static-files': {
        license: '/public/package-data/lnd/0.11.1/LICENSE.md',
        icon: '/assets/img/service-icons/lnd.png',
        instructions: '/public/package-data/lnd/0.11.1/INSTRUCTIONS.md',
      },
      manifest: {
        id: 'lnd',
        title: 'Lightning Network Daemon',
        version: '0.11.0',
        description: {
          short: 'A bolt spec compliant client.',
          long: 'More info about LND. More info about LND. More info about LND.',
        },
        'release-notes': 'Dual funded channels!',
        assets: {
          icon: 'icon.png',
          license: 'LICENSE.md',
          instructions: 'INSTRUCTIONS.md',
          docker_images: 'image.tar',
          assets: './assets',
          scripts: './scripts',
        },
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
        'health-checks': {},
        config: {
          get: null,
          set: null,
        },
        volumes: {},
        'min-os-version': '0.2.12',
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
        dependencies: {
          bitcoind: {
            version: '=0.21.0',
            description: 'LND needs bitcoin to live.',
            requirement: {
              type: 'opt-out',
              how: 'You can use an external node from your server if you prefer.',
            },
            config: null,
          },
          'btc-rpc-proxy': {
            version: '>=0.2.2',
            description:
              'As long as Bitcoin is pruned, LND needs Bitcoin Proxy to fetch block over the P2P network.',
            requirement: {
              type: 'opt-in',
              how: `To use Proxy's user management system, go to LND config and select Bitcoin Proxy under Bitcoin config.`,
            },
            config: null,
          },
        },
      },
      installed: {
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.0',
        },
        'last-backup': null,
        status: {
          configured: true,
          main: {
            status: PackageMainStatus.Stopped,
          },
          'dependency-config-errors': {
            'btc-rpc-proxy': 'This is a config unsatisfied error',
          },
        },
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
                secure: true,
                ssl: true,
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
                secure: true,
                ssl: true,
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
                secure: true,
                ssl: true,
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
      },
    },
  },
}
