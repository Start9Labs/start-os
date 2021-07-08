import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'

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
  error = ''

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
  ) { }

  async ngOnInit () {
    try {
      this.content = await this.apiService.getStatic(this.contentUrl)
    } catch (e) {
      this.error = e.message
    } finally {
      this.loading = false
    }
  }

  async dismiss () {
    return this.modalCtrl.dismiss(true)
  }
}
