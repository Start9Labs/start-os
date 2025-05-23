export type WorkspaceConfig = {
  gitHash: string
  useMocks: boolean
  // each key corresponds to a project and values adjust settings for that project, eg: ui, install-wizard, setup-wizard
  ui: {
    api: {
      url: string
      version: string
    }
    mocks: {
      maskAs: 'tor' | 'local' | 'localhost' | 'ipv4' | 'ipv6' | 'clearnet'
      maskAsHttps: boolean
      skipStartupAlerts: boolean
    }
  }
}

export const defaultRegistries = {
  start9: 'https://registry.start9.com/',
  community: 'https://community-registry.start9.com/',
} as const

export const knownRegistries = {
  ...defaultRegistries,
  start9Alpha: 'https://alpha-registry-x.start9.com/',
  start9Beta: 'https://beta-registry.start9.com/',
} as const
