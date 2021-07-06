import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'app-metrics',
  templateUrl: './app-metrics.page.html',
  styleUrls: ['./app-metrics.page.scss'],
})
export class AppMetricsPage {
  pkg: PackageDataEntry

  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.pkg = this.patch.data['package-data'][pkgId]
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
