import { object, literal, string, boolean, some } from "ts-matches"

const matchDataVolume = object({
  type: literal("data"),
  readonly: boolean.optional(),
})
const matchAssetVolume = object({
  type: literal("assets"),
})
const matchPointerVolume = object({
  type: literal("pointer"),
  "package-id": string,
  "volume-id": string,
  path: string,
  readonly: boolean,
})
const matchCertificateVolume = object({
  type: literal("certificate"),
  "interface-id": string,
})
const matchBackupVolume = object({
  type: literal("backup"),
  readonly: boolean,
})
export const matchVolume = some(
  matchDataVolume,
  matchAssetVolume,
  matchPointerVolume,
  matchCertificateVolume,
  matchBackupVolume,
)
export type Volume = typeof matchVolume._TYPE
