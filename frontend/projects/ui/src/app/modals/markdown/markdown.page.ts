import { Component, Input } from '@angular/core'
import { ModalController, IonicSafeString } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { getErrorMessage } from 'src/app/services/error-toast.service'
import { pauseFor } from '../../../../../shared/src/util/misc.util'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.page.html',
  styleUrls: ['./markdown.page.scss'],
})
export class MarkdownPage {
  @Input() contentUrl?: string
  @Input() content?: string
  @Input() title: string
  loading = true
  loadingError: string | IonicSafeString

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
  ) {}

  async ngOnInit() {
    try {
      if (!this.content) {
        this.content = await this.embassyApi.getStatic(this.contentUrl)
      }
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
  }

  async dismiss() {
    return this.modalCtrl.dismiss(true)
  }
}
