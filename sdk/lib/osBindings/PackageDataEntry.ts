// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { ActionId } from "./ActionId"
import type { ActionMetadata } from "./ActionMetadata"
import type { CurrentDependencies } from "./CurrentDependencies"
import type { DataUrl } from "./DataUrl"
import type { Hosts } from "./Hosts"
import type { PackageState } from "./PackageState"
import type { ServiceInterface } from "./ServiceInterface"
import type { ServiceInterfaceId } from "./ServiceInterfaceId"
import type { Status } from "./Status"

export type PackageDataEntry = {
  stateInfo: PackageState
  status: Status
  registry: string | null
  developerKey: string
  icon: DataUrl
  lastBackup: string | null
  currentDependencies: CurrentDependencies
  actions: { [key: ActionId]: ActionMetadata }
  serviceInterfaces: { [key: ServiceInterfaceId]: ServiceInterface }
  hosts: Hosts
  storeExposedDependents: string[]
}
