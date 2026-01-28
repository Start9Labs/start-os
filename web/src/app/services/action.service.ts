import { inject, Injectable } from '@angular/core'
import { TuiNotificationService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'

export interface ActionOptions {
  loading: string
  success: string
  fail: string
}

@Injectable({ providedIn: 'root' })
export class ActionService {
  private readonly alerts = inject(TuiNotificationService)
  private readonly notifications = inject(TuiNotificationMiddleService)

  async run(
    action: () => Promise<unknown>,
    options: Partial<ActionOptions> = { loading: 'Saving' },
  ): Promise<boolean> {
    const loading = this.notifications.open(options.loading).subscribe()

    try {
      await action()

      if (options.success) {
        this.alerts
          .open(options.success, { appearance: 'positive' })
          .subscribe()
      }

      return true
    } catch (e: any) {
      console.error(e)
      this.alerts
        .open(e.message || options.fail || e, { appearance: 'negative' })
        .subscribe()

      return false
    } finally {
      loading.unsubscribe()
    }
  }
}
