import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ApiService } from './services/api/api.service'
import { ErrorToastService } from './services/error-toast.service'
import { StateService } from './services/state.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  constructor (
    private readonly apiService: ApiService,
    private readonly errorToastService: ErrorToastService,
    private readonly navCtrl: NavController,
    private readonly stateService: StateService,
  ) { }

  async ngOnInit () {
    await this.navCtrl.navigateForward(`/success`)
    return
    try {
      const status = await this.apiService.getStatus()
      if (status.migrating || status['product-key']) {
        this.stateService.hasProductKey = true
        this.stateService.isMigrating = status.migrating
        await this.navCtrl.navigateForward(`/product-key`)
      } else {
        this.stateService.hasProductKey = false
        this.stateService.isMigrating = false
        await this.navCtrl.navigateForward(`/recover`)
      }
    } catch (e) {
      this.errorToastService.present(`${e.message}: ${e.details}`)
    }
  }
}
