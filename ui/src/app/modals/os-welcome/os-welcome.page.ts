import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'os-welcome',
  templateUrl: './os-welcome.page.html',
  styleUrls: ['./os-welcome.page.scss'],
})
export class OSWelcomePage {
  @Input() version: string

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  dismiss () {
    this.modalCtrl.dismiss()
  }
}
