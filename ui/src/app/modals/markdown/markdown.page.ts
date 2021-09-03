import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.page.html',
  styleUrls: ['./markdown.page.scss'],
})
export class MarkdownPage {
  @Input() contentUrl: string
  @Input() title: string
  content: string
  loading = true

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
  ) { }

  async ngOnInit () {
    try {
      this.content = await this.embassyApi.getStatic(this.contentUrl)
      const links = document.links
      for (let i = 0, linksLength = links.length; i < linksLength; i++) {
        if (links[i].hostname != window.location.hostname) {
          links[i].target = '_blank'
          links[i].setAttribute('rel', 'noreferrer')
          links[i].className += ' externalLink'
        }
      }
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  async dismiss () {
    return this.modalCtrl.dismiss(true)
  }
}
