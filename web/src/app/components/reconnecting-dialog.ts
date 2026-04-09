import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnDestroy,
  OnInit,
} from '@angular/core'
import { TuiDialogContext, TuiLoader } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'
import { isNetworkError } from 'src/app/services/network-restart.service'

const POLL_INTERVAL_MS = 3000
const TIMEOUT_MS = 120_000

@Component({
  template: `
    <div class="reconnecting">
      <tui-loader size="l" />
      <p>{{ message }}</p>
    </div>
  `,
  styles: `
    .reconnecting {
      text-align: center;
      padding: 2rem;
    }

    tui-loader {
      margin-bottom: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader],
})
export class ReconnectingDialog implements OnInit, OnDestroy {
  private readonly context =
    injectContext<TuiDialogContext<void, { message: string }>>()
  private readonly api = inject(ApiService)
  private pollId?: ReturnType<typeof setInterval>
  private timeoutId?: ReturnType<typeof setTimeout>

  protected get message(): string {
    return this.context.data.message
  }

  ngOnInit(): void {
    this.poll()
    this.pollId = setInterval(() => this.poll(), POLL_INTERVAL_MS)
    this.timeoutId = setTimeout(() => this.complete(), TIMEOUT_MS)
  }

  ngOnDestroy(): void {
    if (this.pollId) clearInterval(this.pollId)
    if (this.timeoutId) clearTimeout(this.timeoutId)
  }

  private async poll(): Promise<void> {
    try {
      await this.api.systemInfo()
      this.complete()
    } catch (e) {
      if (!isNetworkError(e)) {
        this.complete()
      }
    }
  }

  private complete(): void {
    if (this.pollId) clearInterval(this.pollId)
    if (this.timeoutId) clearTimeout(this.timeoutId)
    this.context.completeWith()
  }
}
