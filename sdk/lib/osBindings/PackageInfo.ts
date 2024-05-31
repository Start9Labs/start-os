// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { Guid } from "./Guid"
import type { PackageVersionInfo } from "./PackageVersionInfo"
import type { Version } from "./Version"

export type PackageInfo = {
  signers: Array<Guid>
  versions: { [key: Version]: PackageVersionInfo }
}
