import { Url } from '@start9labs/shared'

export interface MarketplaceManifest {
  id: string
  title: string
  version: string
  description: {
    short: string
    long: string
  }
  'release-notes': string
  license: string // name
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
}
