import { Component } from '@angular/core'
import { Metrics } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TimeService } from 'src/app/services/time-service'
import { pauseFor, ErrorToastService } from '@start9labs/shared'
import { Subject } from 'rxjs'

@Component({
  selector: 'server-metrics',
  templateUrl: './server-metrics.page.html',
  styleUrls: ['./server-metrics.page.scss'],
})
export class ServerMetricsPage {
  going = false
  metrics$ = new Subject<Metrics>()

  readonly now$ = this.timeService.now$
  readonly uptime$ = this.timeService.uptime$

  constructor(
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly timeService: TimeService,
  ) {}

  async ngOnInit() {
    await this.getMetrics()
    this.startDaemon()
  }

  ngOnDestroy() {
    this.stopDaemon()
  }

  private async startDaemon(): Promise<void> {
    this.going = true
    while (this.going) {
      const startTime = Date.now()
      await this.getMetrics()
      await pauseFor(4000 - Math.max(Date.now() - startTime, 0))
    }
  }

  private stopDaemon() {
    this.going = false
  }

  private async getMetrics(): Promise<void> {
    try {
      const metrics = await this.embassyApi.getServerMetrics({})
      this.metrics$.next(metrics)
    } catch (e: any) {
      this.errToast.present(e)
      this.stopDaemon()
    }
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
