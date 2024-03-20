import { DataModel } from 'src/app/services/patch-db/data-model'
import { Mock } from './api.fixures'

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    'ack-welcome': '1.0.0',
    theme: 'Dark',
    desktop: ['lnd'],
    widgets: [],
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
    country: 'us',
    ui: {
      lanHostname: 'adjective-noun.local',
      torHostname: 'myveryownspecialtoraddress.onion',
      ipInfo: {
        eth0: {
          wireless: false,
          ipv4: '10.0.0.1',
          ipv6: 'FE80:CD00:0000:0CDE:1257:0000:211E:729CD',
        },
      },
      domainInfo: null,
    },
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
      primaryProxies: {
        inbound: null,
        outbound: null,
      },
      outboundProxy: null,
    },
    'last-backup': new Date(new Date().valueOf() - 604800001).toISOString(),
    unreadNotifications: {
      count: 4,
      recent: Mock.Notifications,
    },
    'eos-version-compat': '>=0.3.0 <=0.3.0.1',
    'status-info': {
      'current-backup': null,
      updated: false,
      'update-progress': null,
      restarting: false,
      'shutting-down': false,
    },
    pubkey: 'npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m',
    'ca-fingerprint': 'SHA-256: 63 2B 11 99 44 40 17 DF 37 FC C3 DF 0F 3D 15',
    'ntp-synced': false,
    smtp: {
      server: '',
      port: 587,
      from: '',
      login: '',
      password: '',
    },
    'password-hash':
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
    platform: 'x86_64-nonfree',
  },
  'package-data': {
    bitcoind: {
      ...Mock.bitcoind,
      manifest: {
        ...Mock.bitcoind.manifest,
        version: '0.19.0',
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
    lnd: {
      ...Mock.lnd,
      manifest: {
        ...Mock.lnd.manifest,
        version: '0.11.0',
      },
      icon: '/assets/img/service-icons/lnd.png',
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
      'current-dependents': {},
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
    },
  },
}
