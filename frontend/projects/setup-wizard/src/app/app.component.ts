import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ApiService } from './services/api/api.service'
import { ErrorToastService } from '@start9labs/shared'

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
  ) {}

  async ngOnInit() {
    try {
      const { migrating } = await this.apiService.getStatus()
      await this.navCtrl.navigateForward(migrating ? '/loading' : '/home')
    } catch (e: any) {
      this.errorToastService.present(e)
    }
  }
}
