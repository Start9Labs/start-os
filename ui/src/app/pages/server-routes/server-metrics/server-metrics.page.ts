import { Component, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
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
  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) { }

  ngOnInit () {
    this.startDaemon()
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.stopDaemon()
  }

  async startDaemon (): Promise<void> {
    this.going = true
    while (this.going) {
      await this.getMetrics()
      await pauseFor(250)
    }
  }

  stopDaemon () {
    this.going = false
  }

  async getMetrics (): Promise<void> {
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
    } finally {
      this.loading = false
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
