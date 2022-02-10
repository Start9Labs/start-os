export type WorkspaceConfig = {
  gitHash: string
  useMocks: boolean
  // each key corresponds to a project and values adjust settings for that project, eg: ui, setup-wizard, diagnostic-ui
  ui: {
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
