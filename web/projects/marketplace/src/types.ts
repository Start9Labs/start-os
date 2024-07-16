import { T } from '@start9labs/start-sdk'

export type StoreURL = string
export type StoreName = string

export interface StoreIdentity {
  url: StoreURL
  name?: StoreName
}
export type Marketplace<
  T extends T.GetPackageParams = DefaultGetPackageParams,
> = Record<StoreURL, StoreData<T> | null>

export interface StoreData<T extends T.GetPackageParams> {
  info: T.RegistryInfo
  packages: MarketplacePkg<T>[]
}

export interface DefaultGetPackageParams extends T.GetPackageParams {
  id: null
  version: null
  sourceVersion: null
  otherVersions: 'short'
}

export type StoreIdentityWithData<T extends T.GetPackageParams> = StoreData<T> &
  StoreIdentity

export type MarketplacePkg<
  T extends T.GetPackageParams = DefaultGetPackageParams,
> = MarketplaceMultiPkg<T> | MarketplaceSinglePkg<T>

export type MarketplaceMultiPkg<T extends T.GetPackageParams> = T extends {
  id: null
  otherVersions: null
}
  ? MarketplacePkgInfo & Omit<T.GetPackageResponse, 'otherVersions'>
  : T extends { id: null; otherVersions: 'short' }
  ? MarketplacePkgInfo &
      T.GetPackageResponse & {
        otherVersions: { [version: string]: T.PackageInfoShort }
      }
  : T extends { id: null; otherVersions: 'full' }
  ? MarketplacePkgInfo & T.GetPackageResponseFull
  : never

export type MarketplaceSinglePkg<T extends T.GetPackageParams> = T extends {
  id: T.PackageId
  otherVersions: null
}
  ? MarketplacePkgInfo & Omit<T.GetPackageResponse, 'otherVersions'>
  : T extends { id: T.PackageId; otherVersions: 'short' }
  ? MarketplacePkgInfo &
      T.GetPackageResponse & {
        otherVersions: { [version: string]: T.PackageInfoShort }
      }
  : T extends { id: T.PackageId; otherVersions: 'full' }
  ? MarketplacePkgInfo & T.GetPackageResponseFull
  : never

export interface MarketplacePkgInfo extends T.PackageVersionInfo {
  id: T.PackageId
  version: T.Version
  flavorVersion: T.Version | null
}

export interface Dependency {
  description: string | null
  optional: boolean
}
