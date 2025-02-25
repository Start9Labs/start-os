import { DataModel } from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'
import { knownACME } from 'src/app/utils/acme'
const version = require('../../../../../../package.json').version

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    theme: 'Dark',
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
    id: 'abcdefgh',
    version,
    lastBackup: new Date(new Date().valueOf() - 604800001).toISOString(),
    network: {
      wifi: {
        enabled: false,
        interface: 'wlan0',
        ssids: [],
        selected: null,
        lastRegion: null,
      },
      host: {
        bindings: {
          80: {
            enabled: true,
            net: {
              assignedPort: null,
              assignedSslPort: 443,
              public: false,
            },
            options: {
              preferredExternalPort: 80,
              addSsl: {
                preferredExternalPort: 443,
                alpn: { specified: ['http/1.1', 'h2'] },
              },
              secure: null,
            },
          },
        },
        domains: {},
        onions: ['myveryownspecialtoraddress'],
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
                sslPort: 443,
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
                sslPort: 443,
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
                sslPort: 443,
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
                sslPort: 443,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'eth0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: 'fe80::cd00:0000:0cde:1257:0000:211e:72cd',
                scopeId: 2,
                port: null,
                sslPort: 443,
              },
            },
            {
              kind: 'ip',
              networkInterfaceId: 'wlan0',
              public: false,
              hostname: {
                kind: 'ipv6',
                value: 'fe80::cd00:0000:0cde:1257:0000:211e:1234',
                scopeId: 3,
                port: null,
                sslPort: 443,
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
          ],
        },
      },
      networkInterfaces: {
        eth0: {
          inbound: false,
          outbound: true,
          ipInfo: {
            name: 'Wired Connection 1',
            scopeId: 1,
            deviceType: 'ethernet',
            subnets: ['10.0.0.2/24'],
            wanIp: null,
            ntpServers: [],
          },
        },
        wlan0: {
          inbound: false,
          outbound: true,
          ipInfo: {
            name: 'Wireless Connection 1',
            scopeId: 2,
            deviceType: 'wireless',
            subnets: [
              '10.0.90.12/24',
              'fe80::cd00:0000:0cde:1257:0000:211e:72cd/64',
            ],
            wanIp: null,
            ntpServers: [],
          },
        },
      },
    },
    acme: {
      [knownACME[0].url]: {
        contact: ['mailto:support@start9.com'],
      },
    },
    unreadNotificationCount: 4,
    // password is asdfasdf
    passwordHash:
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    packageVersionCompat: '>=0.3.0 <=0.3.6',
    postInitMigrationTodos: [],
    statusInfo: {
      // currentBackup: null,
      updated: false,
      updateProgress: null,
      restarting: false,
      shuttingDown: false,
      backupProgress: {},
    },
    hostname: 'random-words',
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
    ram: 8 * 1024 * 1024 * 1024,
    devices: [],
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
      lastBackup: new Date(new Date().valueOf() - 604800001).toISOString(),
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
        rpc: {
          name: 'Set RPC',
          description: 'Create RPC Credentials',
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
          bindings: {
            80: {
              enabled: true,
              net: {
                assignedPort: 80,
                assignedSslPort: 443,
                public: false,
              },
              options: {
                addSsl: null,
                preferredExternalPort: 443,
                secure: { ssl: true },
              },
            },
          },
          onions: [],
          domains: {},
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
                  value: 'fe80::cd00:0000:0cde:1257:0000:211e:72cd',
                  scopeId: 2,
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
                  value: 'fe80::cd00:0000:0cde:1257:0000:211e:1234',
                  scopeId: 3,
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
          bindings: {
            8332: {
              enabled: true,
              net: {
                assignedPort: 8332,
                assignedSslPort: null,
                public: false,
              },
              options: {
                addSsl: null,
                preferredExternalPort: 8332,
                secure: { ssl: false },
              },
            },
          },
          onions: [],
          domains: {},
          hostnameInfo: {
            8332: [],
          },
        },
        cdefghi: {
          bindings: {
            8333: {
              enabled: true,
              net: {
                assignedPort: 8333,
                assignedSslPort: null,
                public: false,
              },
              options: {
                addSsl: null,
                preferredExternalPort: 8333,
                secure: { ssl: false },
              },
            },
          },
          onions: [],
          domains: {},
          hostnameInfo: {
            8333: [],
          },
        },
      },
      storeExposedDependents: [],
      registry: 'https://registry.start9.com/',
      developerKey: 'developer-key',
      requestedActions: {
        // 'bitcoind-config': {
        //   request: {
        //     packageId: 'bitcoind',
        //     actionId: 'config',
        //     severity: 'critical',
        //     reason:
        //       'You must run Config before starting Bitcoin for the first time',
        //   },
        //   active: true,
        // },
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
      actions: {
        config: {
          name: 'Config',
          description: 'LND needs configuration before starting',
          warning: null,
          visibility: 'enabled',
          allowedStatuses: 'any',
          hasInput: true,
          group: null,
        },
        connect: {
          name: 'Connect',
          description: 'View LND connection details',
          warning: null,
          visibility: 'enabled',
          allowedStatuses: 'any',
          hasInput: true,
          group: null,
        },
      },
      serviceInterfaces: {
        grpc: {
          id: 'grpc',
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
        config: {
          active: true,
          request: {
            packageId: 'lnd',
            actionId: 'config',
            severity: 'critical',
            reason: 'LND needs configuration before starting',
          },
        },
        connect: {
          active: true,
          request: {
            packageId: 'lnd',
            actionId: 'connect',
            severity: 'important',
            reason: 'View LND connection details',
          },
        },
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
                testnet: false,
              },
            },
          },
        },
        'bitcoind/rpc': {
          active: true,
          request: {
            packageId: 'bitcoind',
            actionId: 'rpc',
            severity: 'important',
            reason: `LND want's its own RPC credentials`,
            input: {
              kind: 'partial',
              value: {
                rpcsettings: {
                  rpcuser: 'lnd',
                },
              },
            },
          },
        },
      },
    },
  },
}
