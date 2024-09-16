import { T } from '@start9labs/start-sdk'

export type GetPackageReq = {
  id: string
  version: string | null
  otherVersions: 'short'
}
export type GetPackageRes = T.GetPackageResponse & {
  otherVersions: { [version: string]: T.PackageInfoShort }
}

export type GetPackagesReq = {
  id: null
  version: null
  otherVersions: 'short'
}

export type GetPackagesRes = {
  [id: T.PackageId]: GetPackageRes
}

export type StoreIdentity = {
  url: string
  name?: string
}

export type Marketplace = Record<string, StoreData | null>

export type StoreData = {
  info: T.RegistryInfo
  packages: MarketplacePkg[]
}

export type MarketplacePkg = T.PackageVersionInfo &
  Omit<GetPackageRes, 'best'> & {
    id: T.PackageId
    version: string
    flavor: string | null
  }
