import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { MarkdownPage } from '../../../modals/markdown/markdown.page'

@Component({
  selector: 'instructions',
  templateUrl: 'instructions.page.html',
  styleUrls: ['instructions.page.scss'],
})
export class InstructionsPage {
  editorOptions = { theme: 'vs-dark', language: 'markdown' }
  code: string = `# Create Instructions using Markdown! :)`

  constructor(private readonly modalCtrl: ModalController) {}

  async submit() {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Instructions Sample',
        content: this.code,
      },
      component: MarkdownPage,
    })

    await modal.present()
  }
}
