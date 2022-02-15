import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { Subscription } from 'rxjs'
import { Metric } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { MainStatus } from 'src/app/services/patch-db/data-model'
import { pauseFor } from '@start9labs/shared'

@Component({
  selector: 'app-metrics',
  templateUrl: './app-metrics.page.html',
  styleUrls: ['./app-metrics.page.scss'],
})
export class AppMetricsPage {
  loading = true
  pkgId: string
  mainStatus: MainStatus
  going = false
  metrics: Metric
  subs: Subscription[] = []

  @ViewChild(IonContent) content: IonContent

  constructor(
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) {}

  ngOnInit() {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.startDaemon()
  }

  ngAfterViewInit() {
    this.content.scrollToPoint(undefined, 1)
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
    } catch (e) {
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
