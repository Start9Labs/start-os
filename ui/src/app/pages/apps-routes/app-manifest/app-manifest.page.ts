import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { Subscription } from 'rxjs'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbModel } from 'src/app/services/patch-db/patch-db.service'
import { getManifest } from 'src/app/services/config.service'
import * as JsonPointer from 'json-pointer'
import { IonContent } from '@ionic/angular'

@Component({
  selector: 'app-manifest',
  templateUrl: './app-manifest.page.html',
  styleUrls: ['./app-manifest.page.scss'],
})
export class AppManifestPage {
  pkg: PackageDataEntry
  pointer: string
  node: object
  segmentValue: 'formatted' | 'raw' = 'formatted'

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
        this.setNode()
      }),
    ]

    this.setNode()
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
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
