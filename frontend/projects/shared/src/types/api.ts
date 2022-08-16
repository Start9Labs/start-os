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
