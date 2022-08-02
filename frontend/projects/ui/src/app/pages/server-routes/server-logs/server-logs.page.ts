import { Component } from '@angular/core'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'server-logs',
  templateUrl: './server-logs.page.html',
  styleUrls: ['./server-logs.page.scss'],
})
export class ServerLogsPage {
  constructor(private readonly embassyApi: ApiService) {}

  followLogs() {
    return async (params: RR.FollowServerLogsReq) => {
      return this.embassyApi.followServerLogs(params)
    }
  }

  fetchLogs() {
    return async (params: RR.GetServerLogsReq) => {
      return this.embassyApi.getServerLogs(params)
    }
  }
}
