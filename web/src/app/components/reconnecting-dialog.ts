import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnDestroy,
  OnInit,
} from '@angular/core'
import { TuiDialogContext, TuiLoader } from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from 'src/app/services/api/api.service'
import { isNetworkError } from 'src/app/services/network-restart.service'

const POLL_INTERVAL_MS = 3000
const TIMEOUT_MS = 120_000

@Component({
  template: '<tui-loader size="l" [textContent]="context.data" />',
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader],
})
export class ReconnectingDialog implements OnInit, OnDestroy {
  private readonly api = inject(ApiService)
  private pollId?: ReturnType<typeof setInterval>
  private timeoutId?: ReturnType<typeof setTimeout>

  protected readonly context = injectContext<TuiDialogContext<void, string>>()

  ngOnInit(): void {
    this.poll()
    this.pollId = setInterval(() => this.poll(), POLL_INTERVAL_MS)
    this.timeoutId = setTimeout(() => this.context.completeWith(), TIMEOUT_MS)
  }

  ngOnDestroy(): void {
    if (this.pollId) clearInterval(this.pollId)
    if (this.timeoutId) clearTimeout(this.timeoutId)
  }

  private async poll(): Promise<void> {
    try {
      await this.api.systemInfo()
      this.context.completeWith()
    } catch (e) {
      if (!isNetworkError(e)) {
        this.context.completeWith()
      }
    }
  }
}

export const RECONNECTING_DIALOG = new PolymorpheusComponent(ReconnectingDialog)
