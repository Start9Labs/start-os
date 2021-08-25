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

  fetchFetchLogs (): Function {
    return async (params: { before?: string, after?: string, limit: number }) => {
      return this.embassyApi.getServerLogs({
        after: params.after,
        before: params.before,
        limit: params.limit,
      })
    }
  }
}
