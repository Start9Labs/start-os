// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { PackageInfoShort } from "./PackageInfoShort"
import type { PackageVersionInfo } from "./PackageVersionInfo"
import type { Version } from "./Version"

export type GetPackageResponse = {
  categories: string[]
  best: { [key: Version]: PackageVersionInfo }
  otherVersions?: { [key: Version]: PackageInfoShort }
}