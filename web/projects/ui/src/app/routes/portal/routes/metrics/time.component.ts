import { DatePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiNotification, TuiTitle } from '@taiga-ui/core'
import { TimeService } from 'src/app/services/time.service'

@Component({
  standalone: true,
  selector: 'metrics-time',
  template: `
    @if (now(); as time) {
      @if (!time.synced) {
        <tui-notification appearance="warning">
          NTP not synced, time could be wrong
        </tui-notification>
      }
      <div tuiTitle>
        <div tuiSubtitle class="g-secondary">
          {{ time.now | date: 'h:mm a z' : 'UTC' }}
        </div>
        <b>{{ time.now | date: 'MMMM d, y' : 'UTC' }}</b>
      </div>
    } @else {
      Loading...
    }
  `,
  styles: `
    :host {
      height: 100%;
      display: flex;
      flex-direction: column;
      justify-content: center;
      gap: 1rem;
      margin-bottom: 1.5rem;
    }

    [tuiTitle] {
      text-align: center;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiNotification, DatePipe, TuiTitle],
})
export class TimeComponent {
  readonly now = toSignal(inject(TimeService).now$)
}
