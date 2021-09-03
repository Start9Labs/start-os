import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'server-logs',
  templateUrl: './server-logs.page.html',
  styleUrls: ['./server-logs.page.scss'],
})
export class ServerLogsPage {
  pkgId: string
  loading = true
  needInfinite = true
  before: string

  constructor (
    private readonly embassyApi: ApiService,
  ) { }

  fetchFetchLogs () {
    return async (params: { before_flag?: boolean, limit?: number, cursor?: string }) => {
      return this.embassyApi.getServerLogs({
        before_flag: params.before_flag,
        cursor: params.cursor,
        limit: params.limit,
      })
    }
  }
}
