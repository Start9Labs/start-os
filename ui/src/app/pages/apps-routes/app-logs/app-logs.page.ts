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

  fetchFetchLogs (): Function {
    return async (params: { after?: string, before?: string, limit: number }) => {
      const pkgId = this.pkgId
      return this.embassyApi.getPackageLogs({
        id: pkgId,
        after: params.after,
        before: params.before,
        limit: params.limit,
      })
    }
  }
}
