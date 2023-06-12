import { Component } from '@angular/core'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'tor-logs',
  templateUrl: './tor-logs.page.html',
  styleUrls: ['./tor-logs.page.scss'],
})
export class TorLogsPage {
  constructor(private readonly api: ApiService) {}

  followLogs() {
    return async (params: RR.FollowServerLogsReq) => {
      return this.api.followTorLogs(params)
    }
  }

  fetchLogs() {
    return async (params: RR.GetServerLogsReq) => {
      return this.api.getTorLogs(params)
    }
  }
}
