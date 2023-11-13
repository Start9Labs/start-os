import { Component, inject } from '@angular/core'
import { NavController } from '@ionic/angular'
import {
  provideSetupLogsService,
  provideSetupService,
} from '@start9labs/shared'

import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  templateUrl: 'loading.page.html',
  providers: [
    provideSetupService(ApiService),
    provideSetupLogsService(ApiService),
  ],
})
export class LoadingPage {
  readonly navCtrl = inject(NavController)
}
