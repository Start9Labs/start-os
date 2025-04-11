// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { AnySignature } from "./AnySignature"
import type { Blake3Commitment } from "./Blake3Commitment"

export type AddAssetParams = {
  version: string
  platform: string
  url: string
  signature: AnySignature
  commitment: Blake3Commitment
}
