import { Component, DOCUMENT, inject, OnInit } from '@angular/core'
import { Router, RouterOutlet } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { TuiRoot } from '@taiga-ui/core'

import { ApiService } from './services/api.service'
import { StateService } from './services/state.service'

@Component({
  selector: 'app-root',
  template: '<tui-root><router-outlet /></tui-root>',
  imports: [TuiRoot, RouterOutlet],
})
export class AppComponent implements OnInit {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)
  private readonly stateService = inject(StateService)
  private readonly document = inject(DOCUMENT)

  async ngOnInit() {
    try {
      // Determine if we're in kiosk mode
      this.stateService.kiosk = ['localhost', '127.0.0.1'].includes(
        this.document.location.hostname,
      )

      // Get pubkey for encryption
      await this.api.getPubKey()

      // Check setup status to determine initial route
      const status = await this.api.getStatus()

      switch (status.status) {
        case 'needs-install':
          await this.router.navigate(['/language'])
          break

        case 'incomplete':
          // Store the data drive info from status
          if (status.guid) {
            this.stateService.dataDriveGuid = status.guid
          }
          this.stateService.attach = status.attach
          await this.router.navigate(['/language'])
          break

        case 'running':
          // Setup is in progress, show loading page
          await this.router.navigate(['/loading'])
          break

        case 'complete':
          // Setup execution finished, show success page
          await this.router.navigate(['/success'])
          break
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }
}
