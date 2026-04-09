import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnDestroy,
  OnInit,
  signal,
} from '@angular/core'
import { TuiDialogContext, TuiLoader } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'
import {
  isNetworkError,
  NetworkRestartService,
} from 'src/app/services/network-restart.service'

@Component({
  template: `
    <div style="text-align: center; padding: 2rem;">
      @if (ssidConfirmed()) {
        <h3>WiFi Restarted</h3>
        <p>
          Connect to
          <strong>{{ ssid }}</strong>
          to continue.
        </p>
      } @else {
        <tui-loader size="l" />
        <p>Restarting WiFi...</p>
      }
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader],
})
export class ReconnectDialog implements OnInit, OnDestroy {
  private readonly context =
    injectContext<
      TuiDialogContext<void, { ssid: string; done: Promise<boolean> }>
    >()
  private readonly api = inject(ApiService)
  private readonly networkRestart = inject(NetworkRestartService)
  private intervalId?: ReturnType<typeof setInterval>
  private networkDropped = false

  readonly ssidConfirmed = signal(false)

  protected get ssid(): string {
    return this.context.data.ssid
  }

  ngOnInit(): void {
    this.networkRestart.suppress()
    this.context.data.done.then(completed => {
      if (completed) this.ssidConfirmed.set(true)
    })
    this.poll()
    this.intervalId = setInterval(() => this.poll(), 3000)
  }

  ngOnDestroy(): void {
    if (this.intervalId) clearInterval(this.intervalId)
  }

  private async poll(): Promise<void> {
    try {
      await this.api.systemInfo()
      if (this.ssidConfirmed() || this.networkDropped) {
        this.cleanup()
        window.location.reload()
      }
    } catch (e) {
      if (isNetworkError(e)) {
        this.networkDropped = true
        this.ssidConfirmed.set(true)
      } else {
        this.cleanup()
        window.location.reload()
      }
    }
  }

  private cleanup(): void {
    if (this.intervalId) clearInterval(this.intervalId)
    this.networkRestart.recovered()
  }
}
