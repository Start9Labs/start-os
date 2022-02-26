import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { ErrorToastService } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'

@Component({
  selector: 'app-release-notes',
  templateUrl: './app-release-notes.page.html',
  styleUrls: ['./app-release-notes.page.scss'],
})
export class AppReleaseNotes {
  @ViewChild(IonContent) content: IonContent
  selected: string
  pkgId: string
  loading = true

  constructor(
    private readonly route: ActivatedRoute,
    public marketplaceService: AbstractMarketplaceService,
    public errToast: ErrorToastService,
  ) {}

  async ngOnInit() {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    try {
      const promises = []
      if (!this.marketplaceService.releaseNotes[this.pkgId]) {
        promises.push(this.marketplaceService.cacheReleaseNotes(this.pkgId))
      }
      if (!this.marketplaceService.pkgs.length) {
        promises.push(this.marketplaceService.load())
      }
      await Promise.all(promises)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  ngAfterViewInit() {
    this.content.scrollToPoint(undefined, 1)
  }

  setSelected(selected: string) {
    if (this.selected === selected) {
      this.selected = null
    } else {
      this.selected = selected
    }
  }

  getDocSize(selected: string) {
    const element = document.getElementById(selected)
    return `${element.scrollHeight}px`
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
