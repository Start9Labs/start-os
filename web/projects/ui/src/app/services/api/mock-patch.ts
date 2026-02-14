import { DataModel } from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'
import { knownAuthorities } from 'src/app/utils/acme'
const version = require('../../../../../../package.json').version

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    registries: {
      'https://registry.start9.com/': 'Start9 Registry',
      'https://community-registry.start9.com/': 'Community Registry',
    },
    startosRegistry: 'https://registry.start9.com/',
    snakeHighScore: 0,
  },
  serverInfo: {
    arch: 'x86_64',
    id: 'abcdefgh',
    version,
    lastBackup: new Date(new Date().valueOf() - 604800001).toISOString(),
    network: {
      wifi: {
        enabled: true,
        interface: 'wlan0',
        ssids: [],
        selected: null,
        lastRegion: null,
      },
      acme: {
        [knownAuthorities[0].url]: {
          contact: ['mailto:support@start9.com'],
        },
      },
      host: {
        bindings: {
          80: {
            enabled: true,
            net: {
              assignedPort: null,
              assignedSslPort: 443,
            },
            addresses: {
              enabled: [],
              disabled: [],
              available: [
                {
                  ssl: true,
                  public: false,
                  host: 'adjective-noun.local',
                  port: 443,
                  metadata: {
                    kind: 'private-domain',
                    gateways: ['eth0', 'wlan0'],
                  },
                },
                {
                  ssl: true,
                  public: false,
                  host: '10.0.0.1',
                  port: 443,
                  metadata: { kind: 'ipv4', gateway: 'eth0' },
                },
                {
                  ssl: true,
                  public: false,
                  host: '10.0.0.2',
                  port: 443,
                  metadata: { kind: 'ipv4', gateway: 'wlan0' },
                },
                {
                  ssl: true,
                  public: false,
                  host: 'fe80::cd00:0000:0cde:1257:0000:211e:72cd',
                  port: 443,
                  metadata: { kind: 'ipv6', gateway: 'eth0', scopeId: 2 },
                },
                {
                  ssl: true,
                  public: false,
                  host: 'fe80::cd00:0000:0cde:1257:0000:211e:1234',
                  port: 443,
                  metadata: { kind: 'ipv6', gateway: 'wlan0', scopeId: 3 },
                },
                {
                  ssl: false,
                  public: false,
                  host: 'abc123def456ghi789jkl012mno345pqr678stu901vwx234yz567abc.onion',
                  port: 80,
                  metadata: { kind: 'plugin', package: 'tor' },
                },
                {
                  ssl: true,
                  public: false,
                  host: 'abc123def456ghi789jkl012mno345pqr678stu901vwx234yz567abc.onion',
                  port: 443,
                  metadata: { kind: 'plugin', package: 'tor' },
                },
              ],
            },
            options: {
              preferredExternalPort: 80,
              addSsl: {
                preferredExternalPort: 443,
                alpn: { specified: ['http/1.1', 'h2'] },
                addXForwardedHeaders: false,
              },
              secure: null,
            },
          },
        },
        publicDomains: {},
        privateDomains: {},
      },
      gateways: {
        eth0: {
          name: null,
          secure: null,
          type: null,
          ipInfo: {
            name: 'Wired Connection 1',
            scopeId: 1,
            deviceType: 'ethernet',
            subnets: ['10.0.0.2/24'],
            wanIp: '203.0.113.45',
            ntpServers: [],
            lanIp: ['10.0.2.12'],
            dnsServers: [],
          },
        },
        wlan0: {
          name: null,
          secure: null,
          type: null,
          ipInfo: {
            name: 'Wireless Connection 1',
            scopeId: 2,
            deviceType: 'wireless',
            subnets: [
              '10.0.90.12/24',
              'fe80::cd00:0000:0cde:1257:0000:211e:72cd/64',
            ],
            wanIp: '203.0.113.45',
            ntpServers: [],
            lanIp: ['10.0.90.12'],
            dnsServers: ['8.8.8.8'],
          },
        },
        wireguard1: {
          name: 'StartTunnel',
          secure: null,
          type: 'inbound-outbound',
          ipInfo: {
            name: 'wireguard1',
            scopeId: 2,
            deviceType: 'wireguard',
            subnets: [
              '10.0.90.12/24',
              'fe80::cd00:0000:0cde:1257:0000:211e:72cd/64',
            ],
            wanIp: '203.0.113.45',
            ntpServers: [],
            lanIp: ['10.0.90.12'],
            dnsServers: ['1.1.1.1'],
          },
        },
        wireguard2: {
          name: 'Mullvad VPN',
          secure: null,
          type: 'outbound-only',
          ipInfo: {
            name: 'wireguard2',
            scopeId: 4,
            deviceType: 'wireguard',
            subnets: [],
            wanIp: '198.51.100.77',
            ntpServers: [],
            lanIp: [],
            dnsServers: ['10.64.0.1'],
          },
        },
      },
      defaultOutbound: 'eth0',
      dns: {
        dhcpServers: ['1.1.1.1', '8.8.8.8'],
        staticServers: null,
      },
    },
    unreadNotificationCount: 5,
    // password is asdfasdf
    passwordHash:
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    packageVersionCompat: '>=0.3.0 <=0.3.6',
    postInitMigrationTodos: {},
    statusInfo: {
      // currentBackup: null,
      updated: false,
      updateProgress: null,
      restarting: false,
      shuttingDown: false,
      backupProgress: null,
    },
    hostname: 'random-words',
    pubkey: 'npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m',
    caFingerprint: '63:2B:11:99:44:40:17:DF:37:FC:C3:DF:0F:3D:15',
    ntpSynced: false,
    smtp: null,
    platform: 'x86_64-nonfree',
    zram: true,
    governor: 'performance',
    ram: 8 * 1024 * 1024 * 1024,
    devices: [],
    kiosk: true,
    language: 'en_US',
    keyboard: {
      layout: 'us',
      keymap: 'us',
      model: null,
      variant: null,
      options: [],
    },
    // keyboard: null,
  },
  packageData: {
    lnd: {
      stateInfo: {
        state: 'installed',
        manifest: {
          ...Mock.MockManifestLnd,
          version: '0.11.0:0.0.1',
        },
      },
      s9pk: '/media/startos/data/package-data/archive/installed/asdfasdf.s9pk',
      icon: '/assets/img/service-icons/lnd.png',
      lastBackup: null,
      statusInfo: {
        desired: { main: 'stopped' },
        error: null,
        health: {},
        started: null,
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
          group: 'Connecting',
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
          title: Mock.BitcoinDep.title,
          icon: Mock.BitcoinDep.icon,
          kind: 'running',
          versionRange: '>=26.0.0',
          healthChecks: [],
        },
        'btc-rpc-proxy': {
          title: Mock.ProxyDep.title,
          icon: Mock.ProxyDep.icon,
          kind: 'running',
          versionRange: '>2.0.0',
          healthChecks: [],
        },
      },
      hosts: {},
      storeExposedDependents: [],
      outboundGateway: null,
      registry: 'https://registry.start9.com/',
      developerKey: 'developer-key',
      tasks: {
        config: {
          active: true,
          task: {
            packageId: 'lnd',
            actionId: 'config',
            severity: 'critical',
            reason: 'LND needs configuration before starting',
          },
        },
        connect: {
          active: true,
          task: {
            packageId: 'lnd',
            actionId: 'connect',
            severity: 'important',
            reason: 'View LND connection details',
          },
        },
        'bitcoind/config': {
          active: true,
          task: {
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
          task: {
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
    bitcoind: {
      stateInfo: {
        state: 'installed',
        manifest: {
          ...Mock.MockManifestBitcoind,
          version: '0.20.0:0-alpha.1',
        },
      },
      s9pk: '/media/startos/data/package-data/archive/installed/asdfasdf.s9pk',
      icon: '/assets/img/service-icons/bitcoin-core.svg',
      lastBackup: new Date(new Date().valueOf() - 604800001).toISOString(),
      statusInfo: {
        desired: { main: 'stopped' },
        error: null,
        health: {},
        started: null,
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
          description: 'edit bitcoin.conf, <b>soo cool!</b>',
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
          visibility: 'hidden',
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
          masked: true,
          name: 'RPC',
          description:
            'Used by dependent services and client wallets for connecting to your node',
          type: 'api',
          addressInfo: {
            username: 'rpcuser',
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
              },
              addresses: {
                enabled: ['203.0.113.45:443'],
                disabled: [],
                available: [
                  {
                    ssl: true,
                    public: false,
                    host: 'adjective-noun.local',
                    port: 443,
                    metadata: {
                      kind: 'private-domain',
                      gateways: ['eth0'],
                    },
                  },
                  {
                    ssl: true,
                    public: false,
                    host: '10.0.0.1',
                    port: 443,
                    metadata: { kind: 'ipv4', gateway: 'eth0' },
                  },
                  {
                    ssl: true,
                    public: false,
                    host: 'fe80::cd00:0cde:1257:211e:72cd',
                    port: 443,
                    metadata: { kind: 'ipv6', gateway: 'eth0', scopeId: 2 },
                  },
                  {
                    ssl: true,
                    public: true,
                    host: '203.0.113.45',
                    port: 443,
                    metadata: { kind: 'ipv4', gateway: 'eth0' },
                  },
                  {
                    ssl: true,
                    public: true,
                    host: 'bitcoin.example.com',
                    port: 443,
                    metadata: { kind: 'public-domain', gateway: 'eth0' },
                  },
                  {
                    ssl: true,
                    public: false,
                    host: '192.168.10.11',
                    port: 443,
                    metadata: { kind: 'ipv4', gateway: 'wlan0' },
                  },
                  {
                    ssl: true,
                    public: false,
                    host: 'fe80::cd00:0cde:1257:211e:1234',
                    port: 443,
                    metadata: { kind: 'ipv6', gateway: 'wlan0', scopeId: 3 },
                  },
                  {
                    ssl: true,
                    public: false,
                    host: 'my-bitcoin.home',
                    port: 443,
                    metadata: {
                      kind: 'private-domain',
                      gateways: ['wlan0'],
                    },
                  },
                  {
                    ssl: false,
                    public: false,
                    host: 'xyz789abc123def456ghi789jkl012mno345pqr678stu901vwx234.onion',
                    port: 80,
                    metadata: { kind: 'plugin', package: 'tor' },
                  },
                  {
                    ssl: true,
                    public: false,
                    host: 'xyz789abc123def456ghi789jkl012mno345pqr678stu901vwx234.onion',
                    port: 443,
                    metadata: { kind: 'plugin', package: 'tor' },
                  },
                ],
              },
              options: {
                addSsl: null,
                preferredExternalPort: 443,
                secure: { ssl: true },
              },
            },
          },
          publicDomains: {
            'bitcoin.example.com': {
              gateway: 'eth0',
              acme: null,
            },
          },
          privateDomains: {
            'my-bitcoin.home': ['wlan0'],
          },
        },
        bcdefgh: {
          bindings: {
            8332: {
              enabled: true,
              net: {
                assignedPort: 8332,
                assignedSslPort: null,
              },
              addresses: {
                enabled: [],
                disabled: [],
                available: [
                  {
                    ssl: false,
                    public: false,
                    host: 'adjective-noun.local',
                    port: 8332,
                    metadata: {
                      kind: 'private-domain',
                      gateways: ['eth0'],
                    },
                  },
                  {
                    ssl: false,
                    public: false,
                    host: '10.0.0.1',
                    port: 8332,
                    metadata: { kind: 'ipv4', gateway: 'eth0' },
                  },
                ],
              },
              options: {
                addSsl: null,
                preferredExternalPort: 8332,
                secure: { ssl: false },
              },
            },
          },
          publicDomains: {},
          privateDomains: {},
        },
        cdefghi: {
          bindings: {
            8333: {
              enabled: true,
              net: {
                assignedPort: 8333,
                assignedSslPort: null,
              },
              addresses: {
                enabled: [],
                disabled: [],
                available: [],
              },
              options: {
                addSsl: null,
                preferredExternalPort: 8333,
                secure: { ssl: false },
              },
            },
          },
          publicDomains: {},
          privateDomains: {},
        },
      },
      storeExposedDependents: [],
      outboundGateway: null,
      registry: 'https://registry.start9.com/',
      developerKey: 'developer-key',
      tasks: {
        // 'bitcoind-config': {
        //   task: {
        //     packageId: 'bitcoind',
        //     actionId: 'config',
        //     severity: 'critical',
        //     reason:
        //       'You must run Config before starting Bitcoin for the first time',
        //   },
        //   active: true,
        // },
        'bitcoind-properties': {
          task: {
            packageId: 'bitcoind',
            actionId: 'properties',
            severity: 'important',
            reason: 'Check out all the info about your Bitcoin node',
          },
          active: true,
        },
      },
    },
  },
}
