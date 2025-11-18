import { Component, inject, DOCUMENT } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api.service'
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
      this.stateService.kiosk = false

      const inProgress = await this.api.getStatus()

      let route = 'home'

      if (inProgress) {
        route = inProgress.status === 'complete' ? '/success' : '/loading'
      }

      await this.router.navigate([route])
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }
}
