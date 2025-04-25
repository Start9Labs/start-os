export type WorkspaceConfig = {
  gitHash: string
  useMocks: boolean
  // each key corresponds to a project and values adjust settings for that project, eg: ui, install-wizard, setup-wizard
  ui: {
    api: {
      url: string
      version: string
    }
    defaultMarketplace: RegistryUrl
    startosRegistry: RegistryUrl
    mocks: {
      maskAs: 'tor' | 'local' | 'localhost' | 'ipv4' | 'ipv6' | 'clearnet'
      maskAsHttps: boolean
      skipStartupAlerts: boolean
    }
  }
}

export const knownMarketplaceUrls = {
  alpha: 'https://alpha-registry-x.start9.com/',
  beta: 'https://beta-registry.start9.com/',
  prod: 'https://registry.start9.com/',
  community: 'https://community-registry.start9.com/',
} as const

export type RegistryUrl =
  | typeof knownMarketplaceUrls.alpha
  | typeof knownMarketplaceUrls.beta
  | typeof knownMarketplaceUrls.prod
