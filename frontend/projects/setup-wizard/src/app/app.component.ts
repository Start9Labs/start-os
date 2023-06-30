import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ApiService } from './services/api/api.service'
import { ErrorService } from '@start9labs/shared'

@Component({
  selector: 'app-root',
  templateUrl: 'app.component.html',
  styleUrls: ['app.component.scss'],
})
export class AppComponent {
  constructor(
    private readonly apiService: ApiService,
    private readonly errorService: ErrorService,
    private readonly navCtrl: NavController,
  ) {}

  async ngOnInit() {
    try {
      const inProgress = await this.apiService.getSetupStatus()

      let route = '/home'
      if (inProgress) {
        route = inProgress.complete ? '/success' : '/loading'
      }

      await this.navCtrl.navigateForward(route)
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }
}
