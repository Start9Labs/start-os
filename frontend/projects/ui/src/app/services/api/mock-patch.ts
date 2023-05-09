import {
  DataModel,
  DependencyErrorType,
  HealthResult,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'
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
    version: '0.3.4',
    country: 'us',
    'last-backup': new Date(new Date().valueOf() - 604800001).toISOString(),
    'lan-address': 'https://adjective-noun.local',
    'tor-address': 'http://myveryownspecialtoraddress.onion',
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
    'wifi-enabled': false,
    'unread-notification-count': 4,
    'eos-version-compat': '>=0.3.0 <=0.3.0.1',
    'status-info': {
      'current-backup': null,
      updated: false,
      'update-progress': null,
      'shutting-down': false,
    },
    hostname: 'random-words',
    pubkey: 'npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m',
    'ca-fingerprint': 'SHA-256: 63 2B 11 99 44 40 17 DF 37 FC C3 DF 0F 3D 15',
    'system-start-time': new Date(new Date().valueOf() - 360042).toUTCString(),
  },
  'package-data': {
    bitcoind: {
      state: PackageState.Installed,
      icon: '/assets/img/service-icons/bitcoind.svg',
      manifest: {
        id: 'bitcoind',
        title: 'Bitcoin Core',
        version: '0.20.0',
        'git-hash': 'abcdefgh',
        description: {
          short: 'A Bitcoin full node by Bitcoin Core.',
          long: 'Bitcoin is a decentralized consensus protocol and settlement network.',
        },
        assets: {
          icon: 'icon.png',
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
          start: 'Starting Bitcoin is good for your health.',
          stop: null,
        },
        dependencies: {},
        'os-version': '0.4.0',
      },
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
    },
    lnd: {
      state: PackageState.Installed,
      icon: '/assets/img/service-icons/lnd.png',
      manifest: {
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
      },
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
    },
  },
}
