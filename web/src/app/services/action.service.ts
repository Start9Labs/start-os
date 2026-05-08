import { inject, Injectable } from '@angular/core'
import { TuiNotificationService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { firstValueFrom, EMPTY, catchError, Subscription } from 'rxjs'
import { ReconnectingDialog } from 'src/app/components/reconnecting-dialog'
import { ApiService } from 'src/app/services/api/api.service'
import { pauseFor } from 'src/app/utils/pauseFor'
import {
  isNetworkError,
  NetworkRestartService,
} from './network-restart.service'

const RESTART_TIMEOUT_MS = 60_000
const POLL_INTERVAL_MS = 2000
const POLL_ATTEMPTS = 3
// Per-poll timeout so a request stuck on a half-broken connection
// (e.g. wedged TCP socket whose conntrack was flushed mid-flight)
// transitions into the reconnecting flow instead of hanging the
// for-loop indefinitely.
const POLL_TIMEOUT_MS = 5000

export interface ActionOptions {
  loading: string
  success: string
  fail: string
  /** Suppress network errors caused by the action restarting network services */
  restart: boolean
  /** Show reconnecting overlay on network error (defaults to restart value) */
  reconnect: boolean
}

@Injectable({ providedIn: 'root' })
export class ActionService {
  private readonly alerts = inject(TuiNotificationService)
  private readonly notifications = inject(TuiNotificationMiddleService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly networkRestart = inject(NetworkRestartService)
  private readonly api = inject(ApiService)

  async run(
    action: () => Promise<unknown>,
    options: Partial<ActionOptions> = { loading: 'Saving' },
  ): Promise<boolean> {
    const loading = this.notifications.open(options.loading).subscribe()

    try {
      if (options.restart) {
        this.networkRestart.suppress()
      }

      await (options.restart
        ? Promise.race([
            action(),
            pauseFor(RESTART_TIMEOUT_MS).then(() => {
              throw Object.assign(new Error('Network timeout'), { code: 0 })
            }),
          ])
        : action())

      // After success, poll until network drops or confirms stable
      const reconnect = options.reconnect ?? options.restart
      if (options.restart && reconnect) {
        await this.pollUntilSettled(
          loading,
          options.loading || 'Reconnecting...',
        )
      } else if (options.restart) {
        this.networkRestart.recovered()
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

        const reconnect = options.reconnect ?? options.restart
        if (reconnect) {
          loading.unsubscribe()
          await this.waitForReconnect(options.loading || 'Reconnecting...')
          this.networkRestart.recovered()
        }

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

  private async pollUntilSettled(
    loading: Subscription,
    message: string,
  ): Promise<void> {
    for (let i = 0; i < POLL_ATTEMPTS; i++) {
      await pauseFor(POLL_INTERVAL_MS)
      try {
        await Promise.race([
          this.api.systemInfo(),
          pauseFor(POLL_TIMEOUT_MS).then(() => {
            throw Object.assign(new Error('Poll timeout'), { code: 0 })
          }),
        ])
      } catch (e) {
        if (isNetworkError(e)) {
          loading.unsubscribe()
          await this.waitForReconnect(message)
          this.networkRestart.recovered()
          return
        }
      }
    }
    // Network stayed up through all polls — restart didn't disrupt connectivity
    this.networkRestart.recovered()
  }

  private waitForReconnect(message: string): Promise<void> {
    return firstValueFrom(
      this.dialogs
        .open(new PolymorpheusComponent(ReconnectingDialog), {
          label: 'Reconnecting',
          closable: false,
          dismissible: false,
          data: { message },
        })
        .pipe(catchError(() => EMPTY)),
    ).then(() => {})
  }
}
