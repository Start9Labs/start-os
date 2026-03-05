import { inject, Injectable } from '@angular/core'
import { TuiNotificationService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import {
  isNetworkError,
  NetworkRestartService,
} from './network-restart.service'

export interface ActionOptions {
  loading: string
  success: string
  fail: string
  /** Suppress network errors caused by the action restarting network services */
  restart: boolean
}

@Injectable({ providedIn: 'root' })
export class ActionService {
  private readonly alerts = inject(TuiNotificationService)
  private readonly notifications = inject(TuiNotificationMiddleService)
  private readonly networkRestart = inject(NetworkRestartService)

  async run(
    action: () => Promise<unknown>,
    options: Partial<ActionOptions> = { loading: 'Saving' },
  ): Promise<boolean> {
    const loading = this.notifications.open(options.loading).subscribe()

    try {
      await action()

      if (options.restart) {
        this.networkRestart.suppress()
      }

      if (options.success) {
        this.alerts
          .open(options.success, { appearance: 'positive' })
          .subscribe()
      }

      return true
    } catch (e: any) {
      if (options.restart && isNetworkError(e)) {
        this.networkRestart.suppress()

        if (options.success) {
          this.alerts
            .open(options.success, { appearance: 'positive' })
            .subscribe()
        }
        return true
      }

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
