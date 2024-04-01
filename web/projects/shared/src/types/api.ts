export type FollowLogsReq = {}
export type FollowLogsRes = {
  startCursor: string
  guid: string
}

export type FetchLogsReq = {
  before: boolean
  cursor?: string
  limit?: number
}

export type FetchLogsRes = {
  entries: Log[]
  startCursor?: string
  endCursor?: string
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
  startOs: StartOSDiskInfo | null
  guid: string | null
}

export type StartOSDiskInfo = {
  version: string
  full: boolean
  passwordHash: string | null
  wrappedKey: string | null
}

export interface SetupStatus {
  bytesTransferred: number
  totalBytes: number | null
  complete: boolean
}
