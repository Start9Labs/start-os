import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'markdown',
  templateUrl: './markdown.page.html',
  styleUrls: ['./markdown.page.scss'],
})
export class MarkdownPage {
  @Input() content: string

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  async dismiss () {
    return this.modalCtrl.dismiss(true)
  }
}
