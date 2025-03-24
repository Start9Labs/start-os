export const SYSTEM_MENU = [
  [
    {
      icon: '@tui.settings',
      item: 'system.outlet.general',
    },
    {
      icon: '@tui.mail',
      item: 'system.outlet.email',
    },
  ],
  [
    {
      icon: '@tui.copy-plus',
      item: 'system.outlet.backup',
    },
    {
      icon: '@tui.database-backup',
      item: 'system.outlet.restore',
    },
  ],
  [
    {
      icon: '@tui.monitor',
      item: 'system.outlet.interfaces',
    },
    {
      icon: '@tui.award',
      item: 'system.outlet.acme',
    },
    {
      icon: '@tui.wifi',
      item: 'system.outlet.wifi',
    },
  ],
  [
    {
      icon: '@tui.clock',
      item: 'system.outlet.sessions',
    },
    {
      icon: '@tui.terminal',
      item: 'system.outlet.ssh',
    },
    {
      icon: '@tui.key',
      item: 'system.outlet.password',
    },
  ],
] as const
