// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { SignerKey } from "./SignerKey"

export type AcceptSigners =
  | { signer: SignerKey }
  | { any: Array<AcceptSigners> }
  | { all: Array<AcceptSigners> }
