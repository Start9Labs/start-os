import { DataModel } from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    ackWelcome: '1.0.0',
    theme: 'Dark',
    desktop: ['lnd'],
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
        interface: 'test',
        ssids: [],
        selected: null,
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
    platform: 'x86_64-nonfree',
    zram: true,
    governor: 'performance',
    passwordHash:
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    arch: 'x86_64',
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
      installedAt: new Date().toISOString(),
      lastBackup: new Date(new Date().valueOf() - 604800001).toISOString(),
      nextBackup: new Date(new Date().valueOf() + 100000000).toISOString(),
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
          visibility: { disabled: 'This is temporarily disabled' },
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
      outboundProxy: null,
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
      installedAt: new Date().toISOString(),
      lastBackup: null,
      nextBackup: null,
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
      outboundProxy: null,
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
