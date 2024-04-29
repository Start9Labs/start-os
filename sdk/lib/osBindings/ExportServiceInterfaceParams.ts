// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { AddressInfo } from "./AddressInfo"
import type { ExportedHostnameInfo } from "./ExportedHostnameInfo"
import type { HostKind } from "./HostKind"
import type { ServiceInterfaceId } from "./ServiceInterfaceId"
import type { ServiceInterfaceType } from "./ServiceInterfaceType"

export type ExportServiceInterfaceParams = {
  id: ServiceInterfaceId
  name: string
  description: string
  hasPrimary: boolean
  disabled: boolean
  masked: boolean
  addressInfo: AddressInfo
  type: ServiceInterfaceType
  hostKind: HostKind
  hostnames: Array<ExportedHostnameInfo>
}
