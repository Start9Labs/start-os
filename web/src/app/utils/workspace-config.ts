import { InjectionToken } from '@angular/core'

export type WorkspaceConfig = {
  useMocks: boolean
  api: {
    url: string
    version: string
  }
}

export const IS_MOCK = new InjectionToken<boolean>('IS_MOCK')
