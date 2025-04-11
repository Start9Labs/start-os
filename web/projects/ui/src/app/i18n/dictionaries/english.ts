export default {
  ui: {
    back: 'Back',
    change: 'Change',
    update: 'Update',
    reset: 'Reset',
  },
  system: {
    outlet: {
      system: 'System',
      general: 'General',
      email: 'Email',
      backup: 'Create Backup',
      restore: 'Restore Backup',
      interfaces: 'StartOS UI',
      acme: 'ACME',
      wifi: 'WiFi',
      sessions: 'Active Sessions',
      password: 'Change Password',
    },
    general: {
      title: 'General Settings',
      subtitle: 'Manage your overall setup and preferences',
      tab: 'Browser Tab Title',
      language: 'Language',
      repair: {
        title: 'Disk Repair',
        subtitle: 'Attempt automatic repair',
        button: 'Repair',
      },
      ca: {
        title: 'Root Certificate Authority',
        subtitle: `Download your server's Root CA`,
        button: 'Download',
      },
      tor: {
        title: 'Reset Tor',
        subtitle: 'Restart the Tor daemon on your server',
      },
      update: {
        title: 'Software Update',
        button: {
          restart: 'Restart to apply',
          check: 'Check for updates',
        },
      },
    },
  },
}
