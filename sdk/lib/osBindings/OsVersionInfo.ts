// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { Blake3Commitment } from "./Blake3Commitment"
import type { Guid } from "./Guid"
import type { RegistryAsset } from "./RegistryAsset"

export type OsVersionInfo = {
  headline: string
  releaseNotes: string
  sourceVersion: string
  signers: Array<Guid>
  iso: { [key: string]: RegistryAsset<Blake3Commitment> }
  squashfs: { [key: string]: RegistryAsset<Blake3Commitment> }
  img: { [key: string]: RegistryAsset<Blake3Commitment> }
}
