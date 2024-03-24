import {
  DataModel,
  HealthResult,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    ackWelcome: '1.0.0',
    theme: 'Dark',
    desktop: ['lnd'],
    widgets: [],
    marketplace: {
      selectedUrl: 'https://registry.start9.com/',
      knownHosts: {
        'https://registry.start9.com/': {
          name: 'Start9 Registry',
        },
        'https://community-registry.start9.com/': {},
        'https://beta-registry.start9.com/': {
          name: 'Dark9',
        },
      },
    },
    gaming: {
      snake: {
        highScore: 0,
      },
    },
    ackInstructions: {},
  },
  serverInfo: {
    id: 'abcdefgh',
    version: '0.3.5.1',
    country: 'us',
    ui: [
      {
        kind: 'ip',
        networkInterfaceId: 'elan0',
        public: false,
        hostname: {
          kind: 'local',
          value: 'adjective-noun.local',
          port: null,
          sslPort: 1111,
        },
      },
      {
        kind: 'onion',
        hostname: {
          value: 'myveryownspecialtoraddress.onion',
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
          sslPort: 1111,
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
          sslPort: 1111,
        },
      },
    ],
    network: {
      domains: [],
      start9ToSubdomain: null,
      wifi: {
        enabled: false,
        lastRegion: null,
      },
      wanConfig: {
        upnp: false,
        forwards: [
          {
            assigned: 443,
            override: null,
            target: 443,
            error: null,
          },
          {
            assigned: 80,
            override: null,
            target: 80,
            error: null,
          },
          {
            assigned: 8332,
            override: null,
            target: 8332,
            error: null,
          },
        ],
      },
      proxies: [],
      outboundProxy: null,
    },
    lastBackup: new Date(new Date().valueOf() - 604800001).toISOString(),
    unreadNotifications: {
      count: 4,
      recent: Mock.Notifications,
    },
    eosVersionCompat: '>=0.3.0 <=0.3.0.1',
    statusInfo: {
      currentBackup: null,
      updated: false,
      updateProgress: null,
      restarting: false,
      shuttingDown: false,
    },
    pubkey: 'npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m',
    caFingerprint: 'SHA-256: 63 2B 11 99 44 40 17 DF 37 FC C3 DF 0F 3D 15',
    ntpSynced: false,
    smtp: {
      server: '',
      port: 587,
      from: '',
      login: '',
      password: '',
    },
    passwordHash:
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    platform: 'x86_64-nonfree',
  },
  packageData: {
    bitcoind: {
      stateInfo: {
        state: PackageState.Installed,
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0',
        },
      },
      icon: '/assets/img/service-icons/bitcoind.svg',
      installedAt: new Date().toISOString(),
      lastBackup: null,
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
              name: 'P2P',
              result: HealthResult.Success,
              message: 'Health check successful',
            },
            'rpc-interface': {
              name: 'RPC',
              result: HealthResult.Failure,
              message: 'RPC interface unreachable.',
            },
            'unnecessary-health-check': {
              name: 'Unnecessary Health Check',
              result: HealthResult.Disabled,
            },
          },
        },
        dependencyConfigErrors: {},
      },
      actions: {}, // @TODO
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
      currentDependencies: {},
      dependencyInfo: {},
      marketplaceUrl: 'https://registry.start9.com/',
      developerKey: 'developer-key',
      outboundProxy: null,
    },
    lnd: {
      stateInfo: {
        state: PackageState.Installed,
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.0',
        },
      },
      icon: '/assets/img/service-icons/lnd.png',
      installedAt: new Date().toISOString(),
      lastBackup: null,
      status: {
        configured: true,
        main: {
          status: PackageMainStatus.Stopped,
        },
        dependencyConfigErrors: {
          'btc-rpc-proxy': 'This is a config unsatisfied error',
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
              secure: { ssl: true },
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
          versionRange: '>=26.0.0',
          healthChecks: [],
        },
        'btc-rpc-proxy': {
          versionRange: '>2.0.0',
          healthChecks: [],
        },
      },
      dependencyInfo: {
        bitcoind: {
          title: 'Bitcoin Core',
          icon: 'assets/img/service-icons/bitcoind.svg',
        },
        'btc-rpc-proxy': {
          title: 'Bitcoin Proxy',
          icon: 'assets/img/service-icons/btc-rpc-proxy.png',
        },
      },
      marketplaceUrl: 'https://registry.start9.com/',
      developerKey: 'developer-key',
      outboundProxy: null,
    },
  },
}
