import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'app-release-notes',
  templateUrl: './app-release-notes.page.html',
  styleUrls: ['./app-release-notes.page.scss'],
})
export class AppReleaseNotesPage {
  @Input() releaseNotes: string
  @Input() version: string

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  dismiss () {
    this.modalCtrl.dismiss()
  }
}
