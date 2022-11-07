export type WorkspaceConfig = {
  targetArch: 'aarch64' | 'x86_64'
  gitHash: string
  useMocks: boolean
  // each key corresponds to a project and values adjust settings for that project, eg: ui, install-wizard, setup-wizard, diagnostic-ui
  ui: {
    api: {
      url: string
      version: string
    }
    mocks: {
      maskAs: 'tor' | 'lan'
      skipStartupAlerts: boolean
    }
  }
}
