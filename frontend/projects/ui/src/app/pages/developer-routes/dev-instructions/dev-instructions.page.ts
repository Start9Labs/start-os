import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { MarkdownPage } from '../../../modals/markdown/markdown.page'

@Component({
  selector: 'dev-instructions',
  templateUrl: 'dev-instructions.page.html',
  styleUrls: ['dev-instructions.page.scss'],
})
export class DevInstructionsPage {
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
