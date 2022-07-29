export abstract class ApiService {
  abstract getError(): Promise<GetErrorRes>
  abstract restart(): Promise<void>
  abstract forgetDrive(): Promise<void>
  abstract repairDisk(): Promise<void>
  abstract getLogs(params: GetLogsReq): Promise<GetLogsRes>
}

export interface GetErrorRes {
  code: number
  message: string
  data: { details: string }
}

export type GetLogsReq = {
  before: boolean
  cursor?: string
  limit?: number
  follow?: boolean // include to receive guid in response for initiating websocket
}
export type GetLogsRes = LogsRes

export type LogsRes = {
  entries: Log[]
  'start-cursor'?: string
  'end-cursor'?: string
  guid?: string // only expected if follow: true on request
}

export interface Log {
  timestamp: string
  message: string
}
