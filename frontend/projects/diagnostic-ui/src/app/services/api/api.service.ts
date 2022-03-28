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
  cursor?: string
  before_flag?: boolean
  limit?: number
}
export type GetLogsRes = LogsRes

export type LogsRes = {
  entries: Log[]
  'start-cursor'?: string
  'end-cursor'?: string
}

export interface Log {
  timestamp: string
  message: string
}
