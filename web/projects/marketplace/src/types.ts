import { OptionalProperty } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

export type GetPackageReq = {
  id: string
  targetVersion: string | null
  otherVersions: 'short'
}
export type GetPackageRes = T.GetPackageResponse & {
  otherVersions: { [version: string]: T.PackageInfoShort }
}

export type GetPackagesReq = {
  id: null
  targetVersion: null
  otherVersions: 'short'
}

export type GetPackagesRes = {
  [id: T.PackageId]: GetPackageRes
}

export type StoreIdentity = {
  url: string
  name: string
}

export type Marketplace = Record<string, StoreData | null>

export type StoreData = {
  info: T.RegistryInfo
  packages: MarketplacePkg[]
}

export type MarketplacePkgBase = OptionalProperty<
  T.PackageVersionInfo,
  's9pk'
> & {
  id: T.PackageId
  version: string
  flavor: string | null
}

export type MarketplacePkg = MarketplacePkgBase &
  GetPackageRes &
  T.PackageVersionInfo

export type StoreDataWithUrl = StoreData & { url: string }
