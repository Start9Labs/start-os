import { Url } from '@start9labs/shared'
import { Manifest } from '../../../../core/startos/bindings/Manifest'

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

export interface Dependency {
  description: string | null
  optional: boolean
}
