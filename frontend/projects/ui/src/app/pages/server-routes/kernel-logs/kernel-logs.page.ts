import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'kernel-logs',
  templateUrl: './kernel-logs.page.html',
  styleUrls: ['./kernel-logs.page.scss'],
})
export class KernelLogsPage {
  pkgId: string
  loading = true
  needInfinite = true
  before: string

  constructor(private readonly embassyApi: ApiService) {}

  fetchFetchLogs() {
    return async (params: {
      before_flag?: boolean
      limit?: number
      cursor?: string
    }) => {
      return this.embassyApi.getKernelLogs({
        before_flag: params.before_flag,
        cursor: params.cursor,
        limit: params.limit,
      })
    }
  }
}
