import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  pkgId: string
  loading = true
  needInfinite = true
  before: string

  constructor (
    private readonly embassyApi: ApiService,
  ) { }

  fetchFetchLogs () {
    return async (params: { before_flag?: boolean, limit?: number, cursor?: string }) => {
      const pkgId = this.pkgId
      return this.embassyApi.getPackageLogs({
        id: pkgId,
        before_flag: params.before_flag,
        cursor: params.cursor,
        limit: params.limit,
      })
    }
  }
}
