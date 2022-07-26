import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { Metric } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { pauseFor, ErrorToastService, getPkgId } from '@start9labs/shared'

@Component({
  selector: 'app-metrics',
  templateUrl: './app-metrics.page.html',
  styleUrls: ['./app-metrics.page.scss'],
})
export class AppMetricsPage {
  loading = true
  readonly pkgId = getPkgId(this.route)
  going = false
  metrics?: Metric

  constructor(
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) {}

  ngOnInit() {
    this.startDaemon()
  }

  ngOnDestroy() {
    this.stopDaemon()
  }

  async startDaemon(): Promise<void> {
    this.going = true
    while (this.going) {
      const startTime = Date.now()
      await this.getMetrics()
      await pauseFor(Math.max(4000 - (Date.now() - startTime), 0))
    }
  }

  stopDaemon() {
    this.going = false
  }

  async getMetrics(): Promise<void> {
    try {
      this.metrics = await this.embassyApi.getPkgMetrics({ id: this.pkgId })
    } catch (e: any) {
      this.errToast.present(e)
      this.stopDaemon()
    } finally {
      this.loading = false
    }
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
