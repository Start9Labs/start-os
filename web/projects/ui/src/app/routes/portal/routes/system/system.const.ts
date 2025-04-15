export const SYSTEM_MENU = [
  [
    {
      icon: '@tui.settings',
      item: 'General',
      link: 'general',
    },
    {
      icon: '@tui.mail',
      item: 'Email',
      link: 'email',
    },
  ],
  [
    {
      icon: '@tui.copy-plus',
      item: 'Create Backup',
      link: 'backup',
    },
    {
      icon: '@tui.database-backup',
      item: 'Restore Backup',
      link: 'restore',
    },
  ],
  [
    {
      icon: '@tui.monitor',
      item: 'StartOS UI',
      link: 'interfaces',
    },
    {
      icon: '@tui.award',
      item: 'ACME',
      link: 'acme',
    },
    {
      icon: '@tui.wifi',
      item: 'WiFi',
      link: 'wifi',
    },
  ],
  [
    {
      icon: '@tui.clock',
      item: 'Active Sessions',
      link: 'sessions',
    },
    {
      icon: '@tui.key',
      item: 'Change Password',
      link: 'password',
    },
  ],
] as const
