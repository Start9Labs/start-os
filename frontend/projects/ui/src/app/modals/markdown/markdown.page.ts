import { Component, Input } from '@angular/core'
import { ModalController, IonicSafeString } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getErrorMessage, pauseFor } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { take } from 'rxjs/operators'
import { BehaviorSubject, Observable } from 'rxjs'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.page.html',
  styleUrls: ['./markdown.page.scss'],
})
export class MarkdownPage {
  @Input() content?: string
  @Input() title: string
  @Input() contentUrl?: string
  loading = true
  loadingError: string | IonicSafeString

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly marketplaceService: AbstractMarketplaceService,
  ) {}

  async ngOnInit() {
    let content$: Observable<string>
    if (this.content) {
      content$ = new BehaviorSubject(this.content)
    } else if (this.contentUrl) {
      content$ = new BehaviorSubject(
        await this.embassyApi.getStatic(this.contentUrl),
      )
    } else {
      content$ =
        this.title === 'license'
          ? this.marketplaceService.getLicense()
          : this.marketplaceService.getInstructions()
    }

    content$.pipe(take(1)).subscribe(async content => {
      this.content = content
      try {
        this.loading = false
        await pauseFor(50)
        const links = document.links
        for (let i = 0, linksLength = links.length; i < linksLength; i++) {
          if (links[i].hostname != window.location.hostname) {
            links[i].target = '_blank'
            links[i].setAttribute('rel', 'noreferrer')
            links[i].className += ' externalLink'
          }
        }
      } catch (e) {
        this.loadingError = getErrorMessage(e)
        this.loading = false
      }
    })
  }

  async dismiss() {
    return this.modalCtrl.dismiss(true)
  }
}
