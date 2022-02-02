export type WorkspaceConfig = {
  useMocks: boolean
  // each key corresponds to a project and values adjust settings for that project, eg: ui, setup-wizard, diagnostic-ui
  ui: {
    gitHash: string
    patchDb: {
      poll: {
        cooldown: number /* in ms */
      }
    }
    api: {
      url: string
      version: string
    }
    mocks: {
      maskAs: 'tor' | 'lan'
      skipStartupAlerts: boolean
    }
    marketplace: {
      url: string
      name: string
    }
  }
}
