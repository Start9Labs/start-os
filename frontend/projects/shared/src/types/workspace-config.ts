export type WorkspaceConfig = {
  packageArch: 'aarch64' | 'x86_64'
  osArch: 'aarch64' | 'x86_64' | 'raspberrypi'
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
      maskAs: 'tor' | 'lan'
      skipStartupAlerts: boolean
    }
  }
}

export interface MarketplaceConfig {
  start9: 'https://registry.start9.com/'
  community: 'https://community-registry.start9.com/'
}
