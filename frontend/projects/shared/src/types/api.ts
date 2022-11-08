export type ServerLogsReq = {
  before: boolean
  cursor?: string
  limit?: number
}

export type LogsRes = {
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
  'embassy-os': EmbassyOSDiskInfo | null
}

export type EmbassyOSDiskInfo = {
  version: string
  full: boolean
  'password-hash': string | null
  'wrapped-key': string | null
}
