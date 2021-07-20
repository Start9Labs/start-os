import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
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
