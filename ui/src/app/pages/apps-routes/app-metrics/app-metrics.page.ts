import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { Subscription } from 'rxjs'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'app-metrics',
  templateUrl: './app-metrics.page.html',
  styleUrls: ['./app-metrics.page.scss'],
})
export class AppMetricsPage {
  pkgId: string
  pkg: PackageDataEntry
  subs: Subscription[]

  constructor (
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')

    this.subs = [
      this.patch.watch$('package-data', this.pkgId)
      .subscribe(pkg => {
        this.pkg = pkg
      }),
    ]
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
