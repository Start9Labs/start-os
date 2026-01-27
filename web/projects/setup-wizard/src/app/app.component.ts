import { Component, inject, DOCUMENT } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { ApiService } from './services/api.service'
import { StateService } from './services/state.service'

@Component({
  selector: 'app-root',
  template: '<tui-root tuiTheme="dark"><router-outlet /></tui-root>',
  standalone: false,
})
export class AppComponent {
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
          // Restore keyboard from status if it was previously set
          if (status.keyboard) {
            this.stateService.keyboard = status.keyboard.layout
          }
          // Start the install flow
          await this.router.navigate(['/language'])
          break

        case 'incomplete':
          // Store the data drive info from status
          this.stateService.dataDriveGuid = status.guid
          this.stateService.attach = status.attach
          // Restore keyboard from status if it was previously set
          if (status.keyboard) {
            this.stateService.keyboard = status.keyboard.layout
          }

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
