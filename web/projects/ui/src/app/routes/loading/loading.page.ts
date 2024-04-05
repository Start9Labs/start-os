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
    <app-initializing (finished)="router.navigate(['login'])" />
  `,
  providers: [
    provideSetupService(ApiService),
    provideSetupLogsService(ApiService),
  ],
  styles: ':host { padding: 1rem; }',
  imports: [InitializingComponent],
})
export default class LoadingPage {
  readonly router = inject(Router)
}
