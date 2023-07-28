import { DataModel } from 'src/app/services/patch-db/data-model'
import { BUILT_IN_WIDGETS } from 'src/app/apps/ui/pages/widgets/built-in/widgets'
import { Mock } from './api.fixures'

export const mockPatchData: DataModel = {
  ui: {
    name: `Matt's Server`,
    'ack-welcome': '1.0.0',
    theme: 'Dark',
    desktop: ['lnd'],
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
      start9MeSubdomain: null,
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
    'unread-notification-count': 4,
    'eos-version-compat': '>=0.3.0 <=0.3.0.1',
    'status-info': {
      'current-backup': null,
      updated: false,
      'update-progress': null,
      'shutting-down': false,
    },
    pubkey: 'npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m',
    'ca-fingerprint': 'SHA-256: 63 2B 11 99 44 40 17 DF 37 FC C3 DF 0F 3D 15',
    'system-start-time': new Date(new Date().valueOf() - 360042).toUTCString(),
    zram: false,
    smtp: {
      server: '',
      port: 587,
      from: '',
      login: '',
      password: '',
      tls: true,
    },
    'password-hash':
      '$argon2d$v=19$m=1024,t=1,p=1$YXNkZmFzZGZhc2RmYXNkZg$Ceev1I901G6UwU+hY0sHrFZ56D+o+LNJ',
  },
  'package-data': {
    bitcoind: Mock.bitcoind,
    lnd: Mock.lnd,
  },
}
