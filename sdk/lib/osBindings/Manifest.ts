// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { Alerts } from "./Alerts"
import type { Dependencies } from "./Dependencies"
import type { Description } from "./Description"
import type { HardwareRequirements } from "./HardwareRequirements"
import type { ImageId } from "./ImageId"
import type { PackageId } from "./PackageId"
import type { Version } from "./Version"
import type { VolumeId } from "./VolumeId"

export type Manifest = {
  id: PackageId
  title: string
  version: Version
  releaseNotes: string
  license: string
  wrapperRepo: string
  upstreamRepo: string
  supportSite: string
  marketingSite: string
  donationUrl: string | null
  description: Description
  images: Array<ImageId>
  assets: Array<VolumeId>
  volumes: Array<VolumeId>
  alerts: Alerts
  dependencies: Dependencies
  hardwareRequirements: HardwareRequirements
  gitHash: string | null
  osVersion: Version
  hasConfig: boolean
}
