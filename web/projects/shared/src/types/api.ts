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
  bootId: string
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
  startOs: Record<string, StartOSDiskInfo>
  guid: string | null
}

export type StartOSDiskInfo = {
  hostname: string
  version: string
  timestamp: string
  passwordHash: string | null
  wrappedKey: string | null
}

export interface SetupStatus {
  bytesTransferred: number
  totalBytes: number | null
  complete: boolean
}
