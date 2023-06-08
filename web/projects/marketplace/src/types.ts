import { Url } from '@start9labs/shared'

export type StoreURL = string
export type StoreName = string
export type StoreIcon = string

export interface StoreIdentity {
  url: StoreURL
  name?: StoreName
  icon?: StoreIcon
}
export type Marketplace = Record<StoreURL, StoreData | null>

export interface StoreData {
  info: StoreInfo
  icon?: StoreIcon
  packages: MarketplacePkg[]
}

export interface StoreInfo {
  name: StoreName
  categories: string[]
}

export interface MarketplacePkg {
  icon: Url
  license: Url
  screenshots?: string[]
  instructions: Url
  manifest: Manifest
  categories: string[]
  versions: string[]
  'dependency-metadata': {
    [id: string]: DependencyMetadata
  }
  'published-at': string
}

export interface DependencyMetadata {
  title: string
  icon: Url
  hidden: boolean
}

export interface Manifest {
  id: string
  title: string
  version: string
  'git-hash'?: string
  description: {
    short: string
    long: string
  }
  assets: {
    icon: Url // filename
  }
  replaces?: string[]
  'release-notes': string
  license: string // name of license
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
  dependencies: Record<string, Dependency>
  'os-version': string
}

export interface Dependency {
  version: string
  requirement:
    | {
        type: 'opt-in'
        how: string
      }
    | {
        type: 'opt-out'
        how: string
      }
    | {
        type: 'required'
      }
  description: string | null
}
