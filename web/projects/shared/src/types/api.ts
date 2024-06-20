export type ServerLogsReq = {
  before: boolean
  cursor?: string
  limit?: number
}

export type LogsRes = {
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
  startOs: StartOSDiskInfo | null
  guid: string | null
}

export type StartOSDiskInfo = {
  version: string
  full: boolean
  passwordHash: string | null
  wrappedKey: string | null
}
