import { Component, inject } from '@angular/core'
import { NavController } from '@ionic/angular'
import {
  InitializingComponent,
  provideSetupLogsService,
  provideSetupService,
} from '@start9labs/shared'

import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  standalone: true,
  template: `
    <app-initializing
      class="ion-page"
      (finished)="navCtrl.navigateForward('/login')"
    ></app-initializing>
  `,
  providers: [
    provideSetupService(ApiService),
    provideSetupLogsService(ApiService),
  ],
  imports: [InitializingComponent],
})
export class LoadingPage {
  readonly navCtrl = inject(NavController)
}
