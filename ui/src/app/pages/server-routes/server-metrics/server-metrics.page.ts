import { Component } from '@angular/core'
import { Metrics } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { pauseFor } from 'src/app/util/misc.util'

@Component({
  selector: 'server-metrics',
  templateUrl: './server-metrics.page.html',
  styleUrls: ['./server-metrics.page.scss'],
})
export class ServerMetricsPage {
  loading = true
  going = false
  metrics: Metrics = { }

  constructor (
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) { }

  async ngOnInit () {
    await this.getMetrics()
    let headersCount = 0
    let rowsCount = 0
    Object.values(this.metrics).forEach(groupVal => {
      headersCount++
      Object.keys(groupVal).forEach(_ => {
        rowsCount++
      })
    })
    const height = headersCount * 54 + rowsCount * 50 + 24 // extra 24 for room at the bottom
    const elem = document.getElementById('metricSection')
    elem.style.height = `${height}px`
    this.startDaemon()
    this.loading = false
  }

  ngOnDestroy () {
    this.stopDaemon()
  }

  private async startDaemon (): Promise<void> {
    this.going = true
    while (this.going) {
      await this.getMetrics()
      await pauseFor(250)
    }
  }

  private stopDaemon () {
    this.going = false
  }

  private async getMetrics (): Promise<void> {
    try {
      const metrics = await this.embassyApi.getServerMetrics({ })
      Object.entries(metrics).forEach(([groupKey, groupVal]) => {
        if (!this.metrics[groupKey]) {
          this.metrics[groupKey] = groupVal
        }
        Object.entries(groupVal).forEach(([key, val]) => {
          this.metrics[groupKey][key] = val
        })
      })
    } catch (e) {
      this.errToast.present(e)
      this.stopDaemon()
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
