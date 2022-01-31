export type WorkspaceConfig = {
  useMocks: boolean
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
  }
}
