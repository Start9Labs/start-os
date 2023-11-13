export type WorkspaceConfig = {
  gitHash: string
  useMocks: boolean
  enableWidgets: boolean
  // each key corresponds to a project and values adjust settings for that project, eg: ui, install-wizard, setup-wizard, diagnostic-ui
  ui: {
    api: {
      url: string
      version: string
    }
    marketplace: {
      start9: 'https://registry.start9.com/'
      community: 'https://community-registry.start9.com/'
    }
    mocks: {
      maskAs: 'tor' | 'local' | 'localhost'
      // enables local development in secure mode
      maskAsHttps: boolean
      skipStartupAlerts: boolean
    }
  }
}
