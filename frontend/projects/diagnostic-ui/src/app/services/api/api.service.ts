import { LogsRes, ServerLogsReq } from '@start9labs/shared'

export abstract class ApiService {
  abstract getError(): Promise<GetErrorRes>
  abstract restart(): Promise<void>
  abstract forgetDrive(): Promise<void>
  abstract repairDisk(): Promise<void>
  abstract getLogs(params: ServerLogsReq): Promise<LogsRes>
}

export interface GetErrorRes {
  code: number
  message: string
  data: { details: string }
}
