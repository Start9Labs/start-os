import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { MarketplaceService } from '../marketplace.service'

@Component({
  selector: 'app-release-notes',
  templateUrl: './app-release-notes.page.html',
  styleUrls: ['./app-release-notes.page.scss'],
})
export class AppReleaseNotes {
  @ViewChild(IonContent) content: IonContent
  selected: string
  pkgId: string

  constructor (
    private readonly route: ActivatedRoute,
    public marketplaceService: MarketplaceService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    const version = this.route.snapshot.paramMap.get('version')
    if (!this.marketplaceService.pkgs[this.pkgId]?.['release-notes']) {
      this.marketplaceService.getPkg(this.pkgId, version)
    }
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  setSelected (selected: string) {
    if (this.selected === selected) {
      this.selected = null
    } else {
      this.selected = selected
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
