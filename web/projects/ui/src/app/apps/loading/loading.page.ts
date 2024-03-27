import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
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
      (finished)="router.navigate(['login'])"
    />
  `,
  providers: [
    provideSetupService(ApiService),
    provideSetupLogsService(ApiService),
  ],
  imports: [InitializingComponent],
})
export class LoadingPage {
  readonly router = inject(Router)
}
