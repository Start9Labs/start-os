export type WorkspaceConfig = {
  gitHash: string
  useMocks: boolean
  enableWidgets: boolean
  // each key corresponds to a project and values adjust settings for that project, eg: ui, install-wizard, setup-wizard
  ui: {
    api: {
      url: string
      version: string
    }
    marketplace: MarketplaceConfig
    mocks: {
      maskAs: 'tor' | 'local' | 'localhost' | 'ipv4' | 'ipv6' | 'clearnet'
      maskAsHttps: boolean
      skipStartupAlerts: boolean
    }
  }
}

export interface MarketplaceConfig {
  start9: 'https://registry.start9.com/'
  community: 'https://community-registry.start9.com/'
}
