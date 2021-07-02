import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { Subscription } from 'rxjs'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'app-metrics',
  templateUrl: './app-metrics.page.html',
  styleUrls: ['./app-metrics.page.scss'],
})
export class AppMetricsPage {
  pkg: PackageDataEntry

  @ViewChild(IonContent) content: IonContent
  subs: Subscription[] = []

  constructor (
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId')

    this.subs = [
      this.patch.watch$('package-data', pkgId)
      .subscribe(pkg => {
        this.pkg = pkg
      }),
    ]
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
