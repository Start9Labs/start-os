import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'os-welcome',
  templateUrl: './os-welcome.page.html',
  styleUrls: ['./os-welcome.page.scss'],
})
export class OSWelcomePage {
  @Input() version: string

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly config: ConfigService,
  ) { }

  async dismiss () {
    this.embassyApi.setDbValue({ pointer: '/welcome-ack', value: this.config.version })
    .catch(console.error)

    // return false to skip subsequent alert modals (e.g. check for updates modals)
    // return true to show subsequent alert modals
    return this.modalCtrl.dismiss(true)
  }
}
