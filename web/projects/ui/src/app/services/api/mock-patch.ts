import { DataModel } from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'
import { BUILT_IN_WIDGETS } from '../../pages/widgets/built-in/widgets'
const version = require('../../../../../../package.json').version

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
    version,
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
    versionCompat: '>=0.3.0 <=0.3.6',
    postInitMigrationTodos: [],
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
    smtp: null,
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
          version: '0.20.0:0',
        },
      },
      dataVersion: '0.20.0:0',
      icon: '/assets/img/service-icons/bitcoind.svg',
      lastBackup: null,
      status: {
        main: 'stopped',
      },
      // status: {
      //   main: 'error',
      //   message: 'Bitcoin is erroring out',
      //   debug: 'This is a complete stack trace for bitcoin',
      //   onRebuild: 'start',
      // },
      actions: {
        config: {
          name: 'Set Config',
          description: 'edit bitcoin.conf',
          warning: null,
          visibility: 'enabled',
          allowedStatuses: 'any',
          hasInput: true,
          group: null,
        },
        properties: {
          name: 'View Properties',
          description: 'view important information about Bitcoin',
          warning: null,
          visibility: 'enabled',
          allowedStatuses: 'any',
          hasInput: false,
          group: null,
        },
        test: {
          name: 'Do Another Thing',
          description:
            'An example of an action that shows a warning and takes no input',
          warning: 'careful running this action',
          visibility: 'enabled',
          allowedStatuses: 'only-running',
          hasInput: false,
          group: null,
        },
      },
      serviceInterfaces: {
        ui: {
          id: 'ui',
          hasPrimary: false,
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
                  value: '10.0.0.1',
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
      requestedActions: {
        'bitcoind-config': {
          request: {
            packageId: 'bitcoind',
            actionId: 'config',
            severity: 'critical',
            reason:
              'You must run Config before starting Bitcoin for the first time',
          },
          active: true,
        },
        'bitcoind-properties': {
          request: {
            packageId: 'bitcoind',
            actionId: 'properties',
            severity: 'important',
            reason: 'Check out all the info about your Bitcoin node',
          },
          active: true,
        },
      },
    },
    lnd: {
      stateInfo: {
        state: 'installed',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.0:0.0.1',
        },
      },
      dataVersion: '0.11.0:0.0.1',
      icon: '/assets/img/service-icons/lnd.png',
      lastBackup: null,
      status: {
        main: 'stopped',
      },
      actions: {},
      serviceInterfaces: {
        grpc: {
          id: 'grpc',
          hasPrimary: false,
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
          masked: false,
          name: 'P2P',
          description:
            'Used for connecting to other nodes on the Bitcoin network',
          type: 'p2p',
          addressInfo: {
            username: null,
            hostId: 'rstuvw',
            internalPort: 8333,
            scheme: 'bitcoin',
            sslScheme: null,
            suffix: '',
          },
        },
      },
      currentDependencies: {
        bitcoind: {
          title: 'Bitcoin Core',
          icon: 'assets/img/service-icons/bitcoind.svg',
          kind: 'running',
          versionRange: '>=26.0.0',
          healthChecks: [],
        },
        'btc-rpc-proxy': {
          title: 'Bitcoin Proxy',
          icon: 'assets/img/service-icons/btc-rpc-proxy.png',
          kind: 'running',
          versionRange: '>2.0.0',
          healthChecks: [],
        },
      },
      hosts: {},
      storeExposedDependents: [],
      registry: 'https://registry.start9.com/',
      developerKey: 'developer-key',
      requestedActions: {
        'bitcoind/config': {
          active: true,
          request: {
            packageId: 'bitcoind',
            actionId: 'config',
            severity: 'critical',
            reason: 'LND likes BTC a certain way',
            input: {
              kind: 'partial',
              value: {
                color: '#ffffff',
                rpcsettings: {
                  rpcuser: 'lnd',
                },
                testnet: false,
              },
            },
          },
        },
      },
    },
  },
}
