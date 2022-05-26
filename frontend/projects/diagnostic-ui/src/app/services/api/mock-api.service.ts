import { Injectable } from '@angular/core'
import { pauseFor } from '@start9labs/shared'
import {
  ApiService,
  GetErrorRes,
  GetLogsReq,
  GetLogsRes,
  Log,
} from './api.service'

@Injectable()
export class MockApiService extends ApiService {
  constructor() {
    super()
  }

  async getError(): Promise<GetErrorRes> {
    await pauseFor(1000)
    return {
      code: 15,
      message: 'Unknown Embassy',
      data: { details: 'Some details about the error here' },
    }
  }

  async restart(): Promise<void> {
    await pauseFor(1000)
  }

  async forgetDrive(): Promise<void> {
    await pauseFor(1000)
  }

  async repairDisk(): Promise<void> {
    await pauseFor(1000)
  }

  async getLogs(params: GetLogsReq): Promise<GetLogsRes> {
    await pauseFor(1000)
    let entries: Log[]
    if (Math.random() < 0.2) {
      entries = packageLogs
    } else {
      const arrLength = params.limit
        ? Math.ceil(params.limit / packageLogs.length)
        : 10
      entries = new Array(arrLength)
        .fill(packageLogs)
        .reduce((acc, val) => acc.concat(val), [])
    }
    return {
      entries,
      'start-cursor': 'startCursor',
      'end-cursor': 'endCursor',
    }
  }
}

const packageLogs = [
  {
    timestamp: '2019-12-26T14:20:30.872Z',
    message: '****** START *****',
  },
  {
    timestamp: '2019-12-26T14:21:30.872Z',
    message: 'ServerLogs ServerLogs ServerLogs ServerLogs ServerLogs',
  },
  {
    timestamp: '2019-12-26T14:22:30.872Z',
    message: '****** FINISH *****',
  },
]
