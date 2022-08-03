import { Component } from '@angular/core'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'kernel-logs',
  templateUrl: './kernel-logs.page.html',
  styleUrls: ['./kernel-logs.page.scss'],
})
export class KernelLogsPage {
  constructor(private readonly embassyApi: ApiService) {}

  followLogs() {
    return async (params: RR.FollowServerLogsReq) => {
      return this.embassyApi.followKernelLogs(params)
    }
  }

  fetchLogs() {
    return async (params: RR.GetServerLogsReq) => {
      return this.embassyApi.getKernelLogs(params)
    }
  }
}
