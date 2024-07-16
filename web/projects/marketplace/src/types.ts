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

// TODO decide if otherVersions should be short or full
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
  ? MarketplacePkgInfo & Omit<GetPackageResponseInterim, 'otherVersions'>
  : T extends { id: null; otherVersions: 'short' }
  ? MarketplacePkgInfo &
      GetPackageResponseInterim & {
        otherVersions: { [version: string]: T.PackageInfoShort }
      }
  : T extends { id: null; otherVersions: 'full' }
  ? MarketplacePkgInfo & GetPackageResponseFullInterim
  : never

export type MarketplaceSinglePkg<T extends T.GetPackageParams> = T extends {
  id: T.PackageId
  otherVersions: null
}
  ? MarketplacePkgInfo & Omit<GetPackageResponseInterim, 'otherVersions'>
  : T extends { id: T.PackageId; otherVersions: 'short' }
  ? MarketplacePkgInfo &
      GetPackageResponseInterim & {
        otherVersions: { [version: string]: T.PackageInfoShort }
      }
  : T extends { id: T.PackageId; otherVersions: 'full' }
  ? MarketplacePkgInfo & GetPackageResponseFullInterim
  : never

export interface MarketplacePkgInfo extends T.PackageVersionInfo {
  id: T.PackageId
  version: T.Version
  altVersion: T.Version | null
}

type UnionOverrideKeys<T, U> = Omit<T, keyof U> & U
// TODO remove when BE types fully support
export interface GetPackageResponseFullInterim
  extends UnionOverrideKeys<
    T.GetPackageResponseFull,
    { best: { [key: T.Version]: T.PackageVersionInfo } }
  > {}

// TODO remove when BE types fully support
export interface GetPackageResponseInterim
  extends UnionOverrideKeys<
    T.GetPackageResponseFull,
    { best: { [key: T.Version]: T.PackageVersionInfo } }
  > {}

export interface Dependency {
  description: string | null
  optional: boolean
}
