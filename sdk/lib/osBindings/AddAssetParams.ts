// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { AnySignature } from "./AnySignature"
import type { Blake3Commitment } from "./Blake3Commitment"
import type { Version } from "./Version"

export type AddAssetParams = {
  version: Version
  platform: string
  upload: boolean
  url: string
  signature: AnySignature
  commitment: Blake3Commitment
}
