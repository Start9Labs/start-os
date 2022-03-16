import { Component, Input, Optional } from '@angular/core'
import { ModalController, IonicSafeString } from '@ionic/angular'

import { getErrorMessage } from '../../services/error-toast.service'
import { pauseFor } from '../../util/misc.util'
import { AbstractApiService } from '../../services/api.service'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.component.html',
  styleUrls: ['./markdown.component.scss'],
})
export class MarkdownComponent {
  @Input() contentUrl?: string
  @Input() content?: string
  @Input() title: string
  loading = true
  loadingError: string | IonicSafeString

  constructor(
    private readonly modalCtrl: ModalController,
    @Optional()
    private readonly embassyApi: AbstractApiService | null,
  ) {}

  async ngOnInit() {
    try {
      if (!this.content) {
        this.content = await this.embassyApi?.getStatic(this.contentUrl)
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
