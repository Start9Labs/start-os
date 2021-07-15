import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { Metric } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { pauseFor } from 'src/app/util/misc.util'

@Component({
  selector: 'app-metrics',
  templateUrl: './app-metrics.page.html',
  styleUrls: ['./app-metrics.page.scss'],
})
export class AppMetricsPage {
  loading = true
  pkgId: string
  pkg: PackageDataEntry
  going = false
  metrics: Metric

  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly patch: PatchDbService,
    private readonly apiService: ApiService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.pkg = this.patch.data['package-data'][this.pkgId]

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
      this.metrics = await this.apiService.getPkgMetrics({ id: this.pkgId})
    } catch (e) {
      console.error(e)
      this.errToast.present(e.message)
      this.stopDaemon()
    } finally {
      this.loading = false
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
