import { Url } from '@start9labs/shared'
import { Dependency } from './dependency'

export interface MarketplaceManifest<T = unknown> {
  id: string
  title: string
  version: string
  description: {
    short: string
    long: string
  }
  'release-notes': string
  license: string // type of license
  'wrapper-repo': Url
  'upstream-repo': Url
  'support-site': Url
  'marketing-site': Url
  'donation-url': Url | null
  alerts: {
    install: string | null
    uninstall: string | null
    restore: string | null
    start: string | null
    stop: string | null
  }
  dependencies: Record<string, Dependency<T>>
}
