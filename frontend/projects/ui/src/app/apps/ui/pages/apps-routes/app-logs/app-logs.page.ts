import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { RR } from 'src/app/services/api/api.types'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  readonly pkgId = getPkgId(this.route)

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
  ) {}

  followLogs() {
    return async (params: RR.FollowServerLogsReq) => {
      return this.embassyApi.followPackageLogs({
        id: this.pkgId,
        ...params,
      })
    }
  }

  fetchLogs() {
    return async (params: RR.GetServerLogsReq) => {
      return this.embassyApi.getPackageLogs({
        id: this.pkgId,
        ...params,
      })
    }
  }
}
