import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ApiService } from './services/api/api.service'
import { ErrorToastService } from '@start9labs/shared'
import { StateService } from './services/state.service'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  constructor(
    private readonly apiService: ApiService,
    private readonly errorToastService: ErrorToastService,
    private readonly navCtrl: NavController,
    private readonly stateService: StateService,
  ) {}

  async ngOnInit() {
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
    } catch (e: any) {
      this.errorToastService.present(e)
    }
  }
}
