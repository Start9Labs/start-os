import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
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
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
  }

  fetchFetchLogs () {
    return async (params: { before_flag?: boolean, limit?: number, cursor?: string }) => {
      return this.embassyApi.getPackageLogs({
        id: this.pkgId,
        before_flag: params.before_flag,
        cursor: params.cursor,
        limit: params.limit,
      })
    }
  }
}
