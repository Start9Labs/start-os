export const MENU = {
  Internet: [
    {
      name: 'WAN Settings',
      icon: '@tui.globe',
      link: 'wan',
    },
    {
      name: 'Published Ports',
      icon: '@tui.globe-lock',
      link: 'published-ports',
    },
    {
      name: 'Outbound VPNs',
      icon: '@tui.shield',
      link: 'outbound',
    },
  ],
  Network: [
    {
      name: 'LAN Settings',
      icon: '@tui.network',
      link: 'lan',
    },
    {
      name: 'Devices',
      icon: '@tui.monitor-smartphone',
      link: 'devices',
    },
  ],
  'Security Profiles': [
    {
      name: 'Profiles',
      icon: '@tui.scroll-text',
      link: 'profiles',
    },
  ],
  'Points of entry': [
    {
      name: 'Ethernet',
      icon: '@tui.ethernet-port',
      link: 'ethernet',
    },
    {
      name: 'Wi-Fi',
      icon: '@tui.wifi',
      link: 'wifi',
    },
    {
      name: 'Inbound VPNs',
      icon: '@tui.hard-drive-download',
      link: 'inbound',
    },
  ],
  System: [
    {
      name: 'Settings',
      icon: '@tui.settings',
      link: 'settings',
    },
  ],
} as const
