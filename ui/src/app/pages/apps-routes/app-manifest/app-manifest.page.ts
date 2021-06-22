import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { NavController } from '@ionic/angular'
import * as JsonPointer from 'json-pointer'
import { Subscription } from 'rxjs'
import { distinctUntilChanged } from 'rxjs/operators'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { getManifest } from 'src/app/services/config.service'

@Component({
  selector: 'app-manifest',
  templateUrl: './app-manifest.page.html',
  styleUrls: ['./app-manifest.page.scss'],
})
export class AppManifestPage {
  pkgId: string
  pkg: PackageDataEntry
  pointer: string
  node: object
  subs: Subscription[]

  constructor (
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDbModel,
    private readonly navCtrl: NavController,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')

    this.subs = [
      this.patch.watch$('package-data', this.pkgId)
      .subscribe(pkg => {
        this.pkg = pkg
        this.setNode()
      }),
      this.route.queryParams
      .pipe(distinctUntilChanged())
      .subscribe(queryParams => {
        this.pointer = queryParams['pointer']
        this.setNode()
      }),
    ]

    this.setNode()
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  setNode () {
    this.node = JsonPointer.get(getManifest(this.pkg), this.pointer || '')
  }

  async goToNested (key: string): Promise<any> {
    this.navCtrl.navigateForward(`/services/installed/${this.pkgId}/manifest`, {
      queryParams: {
        pointer: `${this.pointer || ''}/${key}`,
      },
    })
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
