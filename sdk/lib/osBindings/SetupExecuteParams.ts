// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { EncryptedWire } from "./EncryptedWire"
import type { RecoverySource } from "./RecoverySource"

export type SetupExecuteParams = {
  startOsLogicalname: string
  startOsPassword: EncryptedWire
  recoverySource: RecoverySource | null
  recoveryPassword: EncryptedWire | null
}