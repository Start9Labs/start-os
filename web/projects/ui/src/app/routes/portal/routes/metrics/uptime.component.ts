import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TimeService } from 'src/app/services/time.service'

@Component({
  standalone: true,
  selector: 'metrics-uptime',
  template: `
    @if (uptime(); as time) {
      <div>
        <b>{{ time.days }}</b>
        Days
      </div>
      <div>
        <b>{{ time.hours }}</b>
        Hours
      </div>
      <div>
        <b>{{ time.minutes }}</b>
        Minutes
      </div>
      <div>
        <b>{{ time.seconds }}</b>
        Seconds
      </div>
    } @else {
      Loading...
    }
  `,
  styles: `
    :host {
      height: 100%;
      min-height: var(--tui-height-l);
      display: flex;
      text-align: center;
      justify-content: center;
      align-items: center;
      gap: 1rem;
      margin-bottom: 1.5rem;
    }

    b {
      display: block;
      font: var(--tui-font-heading-5);
    }

    :host-context(tui-root._mobile) {
      font: var(--tui-font-text-ui-xs);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class UptimeComponent {
  readonly uptime = toSignal(inject(TimeService).uptime$)
}
