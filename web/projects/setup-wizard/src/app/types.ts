import { DiskInfo, PartitionInfo, StartOSDiskInfo } from '@start9labs/shared'

// === Install OS === (no binding available)

export interface InstallOsParams {
  osDrive: string // e.g. /dev/sda
  dataDrive: {
    logicalname: string // e.g. /dev/sda, /dev/sdb3
    wipe: boolean
  }
}

export interface InstallOsRes {
  guid: string // data drive guid
  attach: boolean
}

// === Disk Info Helpers ===

export type StartOSDiskInfoWithId = StartOSDiskInfo & {
  id: string
}

export type StartOSDiskInfoFull = StartOSDiskInfoWithId & {
  partition: PartitionInfo
  drive: DiskInfo
}
