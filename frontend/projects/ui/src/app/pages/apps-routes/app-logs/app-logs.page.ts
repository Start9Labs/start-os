import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  readonly pkgId = getPkgId(this.route)
  loading = true
  needInfinite = true
  before: string

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
  ) {}

  fetchFetchLogs() {
    return async (params: {
      before_flag?: boolean
      limit?: number
      cursor?: string
    }) => {
      return this.embassyApi.getPackageLogs({
        id: this.pkgId,
        before_flag: params.before_flag,
        cursor: params.cursor,
        limit: params.limit,
      })
    }
  }
}
