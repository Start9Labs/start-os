import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ErrorService } from '@start9labs/shared'
import { ApiService } from './services/api/api.service'

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
      const inProgress = await this.apiService.getStatus()

      let route = '/home'
      if (inProgress) {
        route = inProgress.status === 'complete' ? '/success' : '/loading'
      }

      await this.navCtrl.navigateForward(route)
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }
}
