import { DataModel } from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'
import { BUILT_IN_WIDGETS } from '../../pages/widgets/built-in/widgets'

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    ackWelcome: '1.0.0',
    theme: 'Dark',
    widgets: BUILT_IN_WIDGETS.filter(
      ({ id }) =>
        id === 'favorites' ||
        id === 'health' ||
        id === 'network' ||
        id === 'metrics',
    ),
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
    arch: 'x86_64',
    onionAddress: 'myveryownspecialtoraddress',
    id: 'abcdefgh',
    version: '0.3.5.2',
    lastBackup: new Date(new Date().valueOf() - 604800001).toISOString(),
    lanAddress: 'https://adjective-noun.local',
    torAddress: 'https://myveryownspecialtoraddress.onion',
    ipInfo: {
      eth0: {
        ipv4: '10.0.0.1',
        ipv4Range: '10.0.0.1/24',
        ipv6: null,
        ipv6Range: null,
      },
      wlan0: {
        ipv4: '10.0.90.12',
        ipv4Range: '10.0.90.12/24',
        ipv6: 'FE80:CD00:0000:0CDE:1257:0000:211E:729CD',
        ipv6Range: 'FE80:CD00:0000:0CDE:1257:0000:211E:729CD/64',
      },
    },
    unreadNotificationCount: 4,
    // password is asdfasdf
    passwordHash:
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    eosVersionCompat: '>=0.3.0 <=0.3.0.1',
    statusInfo: {
      backupProgress: null,
      updated: false,
      updateProgress: null,
      restarting: false,
      shuttingDown: false,
    },
    hostname: 'random-words',
    pubkey: 'npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m',
    caFingerprint: 'SHA-256: 63 2B 11 99 44 40 17 DF 37 FC C3 DF 0F 3D 15',
    ntpSynced: false,
    platform: 'x86_64-nonfree',
    zram: true,
    governor: 'performance',
    wifi: {
      interface: 'wlan0',
      ssids: [],
      selected: null,
      lastRegion: null,
    },
  },
  packageData: {
    bitcoind: {
      stateInfo: {
        state: 'installed',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0',
        },
      },
      icon: '/assets/img/service-icons/bitcoind.svg',
      lastBackup: null,
      status: {
        configured: true,
        main: {
          status: 'running',
          started: '2021-06-14T20:49:17.774Z',
          health: {
            'ephemeral-health-check': {
              name: 'Ephemeral Health Check',
              result: 'starting',
              message: null,
            },
            'chain-state': {
              name: 'Chain State',
              result: 'loading',
              message: 'Bitcoin is syncing from genesis',
            },
            'p2p-interface': {
              name: 'P2P',
              result: 'success',
              message: 'Health check successful',
            },
            'rpc-interface': {
              name: 'RPC',
              result: 'failure',
              message: 'RPC interface unreachable.',
            },
            'unnecessary-health-check': {
              name: 'Unnecessary Health Check',
              result: 'disabled',
              message: null,
            },
          },
        },
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
                // addXForwardedHeaders: false,
                preferredExternalPort: 443,
                scheme: 'https',
                alpn: { specified: ['http/1.1', 'h2'] },
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
                // addXForwardedHeaders: false,
                preferredExternalPort: 443,
                scheme: 'https',
                alpn: { specified: ['http/1.1'] },
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
      hosts: {},
      storeExposedDependents: [],
      registry: 'https://registry.start9.com/',
      developerKey: 'developer-key',
    },
    lnd: {
      stateInfo: {
        state: 'installed',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.0',
        },
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
          title: 'Bitcoin Core',
          icon: 'assets/img/service-icons/bitcoind.svg',
          kind: 'running',
          registryUrl: 'https://registry.start9.com',
          versionSpec: '>=26.0.0',
          healthChecks: [],
          configSatisfied: true,
        },
        'btc-rpc-proxy': {
          title: 'Bitcoin Proxy',
          icon: 'assets/img/service-icons/btc-rpc-proxy.png',
          kind: 'running',
          registryUrl: 'https://community-registry.start9.com',
          versionSpec: '>2.0.0',
          healthChecks: [],
          configSatisfied: false,
        },
      },
      hosts: {},
      storeExposedDependents: [],
      registry: 'https://registry.start9.com/',
      developerKey: 'developer-key',
    },
  },
}
