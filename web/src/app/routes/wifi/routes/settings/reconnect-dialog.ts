import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnDestroy,
  OnInit,
} from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'
import {
  isNetworkError,
  NetworkRestartService,
} from 'src/app/services/network-restart.service'

@Component({
  template: `
    <div style="text-align: center; padding: 2rem;">
      <h3>WiFi Restarted</h3>
      <p>
        Connect to
        <strong>{{ ssid }}</strong>
        to continue.
      </p>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [],
})
export class ReconnectDialog implements OnInit, OnDestroy {
  private readonly context =
    injectContext<TuiDialogContext<void, { ssid: string }>>()
  private readonly api = inject(ApiService)
  private readonly networkRestart = inject(NetworkRestartService)
  private intervalId?: ReturnType<typeof setInterval>

  protected get ssid(): string {
    return this.context.data.ssid
  }

  ngOnInit(): void {
    this.networkRestart.suppress(Infinity)
    this.intervalId = setInterval(() => this.poll(), 3000)
  }

  ngOnDestroy(): void {
    if (this.intervalId) clearInterval(this.intervalId)
  }

  private async poll(): Promise<void> {
    try {
      await this.api.wifiGet()
      this.reload()
    } catch (e) {
      if (!isNetworkError(e)) {
        this.reload()
      }
    }
  }

  private reload(): void {
    if (this.intervalId) clearInterval(this.intervalId)
    window.location.reload()
  }
}
