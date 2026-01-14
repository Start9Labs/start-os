export const MENU = {
  Internet: [
    {
      name: 'WAN Settings',
      icon: '@tui.globe',
      link: 'wan',
    },
    {
      name: 'Outbound VPNs',
      icon: '@tui.globe-lock',
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
    {
      name: 'Port Forwarding',
      icon: '@tui.chevrons-right',
      link: 'forwarding',
    },
    {
      name: 'IPv6 Firewall',
      icon: '@tui.brick-wall-fire',
      link: 'forwarding',
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
