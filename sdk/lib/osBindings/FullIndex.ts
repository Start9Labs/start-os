// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { DataUrl } from "./DataUrl"
import type { Guid } from "./Guid"
import type { OsIndex } from "./OsIndex"
import type { PackageIndex } from "./PackageIndex"
import type { SignerInfo } from "./SignerInfo"

export type FullIndex = {
  icon: DataUrl | null
  package: PackageIndex
  os: OsIndex
  signers: { [key: Guid]: SignerInfo }
}
