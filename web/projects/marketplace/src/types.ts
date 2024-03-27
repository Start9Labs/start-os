import { Url } from '@start9labs/shared'

export type StoreURL = string
export type StoreName = string

export interface StoreIdentity {
  url: StoreURL
  name?: StoreName
}
export type Marketplace = Record<StoreURL, StoreData | null>

export interface StoreData {
  info: StoreInfo
  packages: MarketplacePkg[]
}

export interface StoreInfo {
  name: StoreName
  categories: string[]
}

export type StoreIdentityWithData = StoreData & StoreIdentity

export interface MarketplacePkg {
  icon: Url
  license: Url
  screenshots?: string[]
  instructions: Url
  manifest: Manifest
  categories: string[]
  versions: string[]
  dependencyMetadata: {
    [id: string]: DependencyMetadata
  }
  publishedAt: string
}

export interface DependencyMetadata {
  title: string
  icon: Url
  optional: boolean
  hidden: boolean
}

export interface Manifest {
  id: string
  title: string
  version: string
  gitHash?: string
  description: {
    short: string
    long: string
  }
  replaces?: string[]
  releaseNotes: string
  license: string // name of license
  wrapperRepo: Url
  upstreamRepo: Url
  supportSite: Url
  marketingSite: Url
  donationUrl: Url | null
  alerts: {
    install: string | null
    uninstall: string | null
    restore: string | null
    start: string | null
    stop: string | null
  }
  dependencies: Record<string, Dependency>
  osVersion: string
  hasConfig: boolean
}

export interface Dependency {
  description: string | null
  optional: boolean
}
