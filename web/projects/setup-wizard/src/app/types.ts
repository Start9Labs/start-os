import {
  DiskInfo,
  FullKeyboard,
  PartitionInfo,
  StartOSDiskInfo,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

// === Echo ===

export type EchoReq = {
  message: string
}

// === Setup Status ===

export type SetupStatusRes =
  | { status: 'needs-install'; keyboard: FullKeyboard | null }
  | {
      status: 'incomplete'
      guid: string
      attach: boolean
      keyboard: FullKeyboard | null
    }
  | { status: 'running'; progress: T.FullProgress; guid: string }
  | { status: 'complete' }

// === Install OS ===

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

// === Attach ===

export interface AttachParams {
  password: T.EncryptedWire | null
  guid: string // data drive
}

// === Execute ===

export interface SetupExecuteParams {
  guid: string
  password: T.EncryptedWire | null // null = keep existing password (for restore/transfer)
  recoverySource:
    | {
        type: 'migrate'
        guid: string
      }
    | {
        type: 'backup'
        target:
          | { type: 'disk'; logicalname: string }
          | {
              type: 'cifs'
              hostname: string
              path: string
              username: string
              password: string | null
            }
        password: T.EncryptedWire
        serverId: string
      }
    | null
}

// === Complete ===

export interface SetupCompleteRes {
  hostname: string // unique.local
  rootCa: string
  needsRestart: boolean
}

// === Disk Info Helpers ===

export type StartOSDiskInfoWithId = StartOSDiskInfo & {
  id: string
}

export type StartOSDiskInfoFull = StartOSDiskInfoWithId & {
  partition: PartitionInfo
  drive: DiskInfo
}
