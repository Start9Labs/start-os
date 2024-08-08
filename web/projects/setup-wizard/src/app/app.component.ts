import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api.service'

@Component({
  selector: 'app-root',
  template: '<tui-root tuiTheme="dark"><router-outlet /></tui-root>',
})
export class AppComponent {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly router = inject(Router)

  async ngOnInit() {
    try {
      const inProgress = await this.api.getSetupStatus()

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
