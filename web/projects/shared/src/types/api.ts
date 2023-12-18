export type FollowLogsReq = {}
export type FollowLogsRes = {
  'start-cursor': string
  guid: string
}

export type FetchLogsReq = {
  before: boolean
  cursor?: string
  limit?: number
}

export type FetchLogsRes = {
  entries: Log[]
  'start-cursor'?: string
  'end-cursor'?: string
}

export interface Log {
  timestamp: string
  message: string
}

export type DiskListResponse = DiskInfo[]

export interface DiskInfo {
  logicalname: string
  vendor: string | null
  model: string | null
  partitions: PartitionInfo[]
  capacity: number
  guid: string | null
}

export interface PartitionInfo {
  logicalname: string
  label: string | null
  capacity: number
  used: number | null
  'embassy-os': StartOSDiskInfo | null
  guid: string | null
}

export type StartOSDiskInfo = {
  version: string
  full: boolean
  'password-hash': string | null
  'wrapped-key': string | null
}

export interface SetupStatus {
  'bytes-transferred': number
  'total-bytes': number | null
  complete: boolean
}