import { Url } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

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
  manifest: T.Manifest
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

export interface Dependency {
  description: string | null
  optional: boolean
}
