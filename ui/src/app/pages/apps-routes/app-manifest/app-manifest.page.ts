import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { Subscription } from 'rxjs'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { getManifest } from 'src/app/services/config.service'
import * as JsonPointer from 'json-pointer'

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
  segmentValue: 'formatted' | 'raw' = 'formatted'

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
        this.setNode()
      }),
    ]

    this.setNode()
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  handleFormattedBack () {
    const arr = this.pointer.split('/')
    arr.pop()
    this.pointer = arr.join('/')
    this.setNode()
  }

  private setNode () {
    this.node = JsonPointer.get(getManifest(this.pkg), this.pointer || '')
  }

  async goToNested (key: string): Promise<any> {
    this.pointer = `${this.pointer || ''}/${key}`
    this.setNode()
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
