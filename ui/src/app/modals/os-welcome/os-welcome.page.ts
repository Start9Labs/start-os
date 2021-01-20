import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { ServerModel } from 'src/app/models/server-model'
import { ApiService } from 'src/app/services/api/api.service'
import { LoaderService } from 'src/app/services/loader.service'
import { pauseFor } from 'src/app/util/misc.util'

@Component({
  selector: 'os-welcome',
  templateUrl: './os-welcome.page.html',
  styleUrls: ['./os-welcome.page.scss'],
})
export class OSWelcomePage {
  @Input() version: string

  autoCheckUpdates = true

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
    private readonly serverModel: ServerModel,
    private readonly loader: LoaderService,
  ) { }

  async dismiss () {
    await this.loader.displayDuringP(
      this.apiService
        .patchServerConfig('autoCheckUpdates', this.autoCheckUpdates)
        .then(() => this.serverModel.update({ autoCheckUpdates: this.autoCheckUpdates }))
        .then(() => pauseFor(600000))
        .catch(console.error),
    ).then(
      () => this.modalCtrl.dismiss({ autoCheckUpdates: this.autoCheckUpdates })
    )
  }
}
