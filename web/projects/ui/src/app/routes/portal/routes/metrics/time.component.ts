import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  TuiHint,
  TuiIcon,
  TuiLink,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { TimeService } from 'src/app/services/time.service'

@Component({
  standalone: true,
  selector: 'metrics-time',
  template: `
    @if (now(); as time) {
      <div tuiCell>
        <div tuiTitle [style.text-align]="'center'">
          <div tuiSubtitle class="g-secondary">
            {{ time.now | date: 'h:mm a z' : 'UTC' }}
          </div>
          <b>{{ time.now | date: 'MMMM d, y' : 'UTC' }}</b>
        </div>
        @if (!time.synced) {
          <tui-icon
            icon="@tui.circle-alert"
            class="g-warning"
            [tuiHint]="hint"
          />
        }
      </div>
      @if (!time.synced) {
        <tui-notification size="s" appearance="warning">
          <ng-container *ngTemplateOutlet="hint" />
        </tui-notification>
      }
    } @else {
      Loading...
    }
    <ng-template #hint>
      <div tuiTitle>
        Clock sync failure
        <div tuiSubtitle>
          To resolve it, refer to
          <a
            tuiLink
            iconEnd="@tui.external-link"
            appearance=""
            href="https://docs.start9.com/0.3.5.x/support/common-issues#clock-sync-failure"
            target="_blank"
            rel="noreferrer"
            [pseudo]="true"
            [textContent]="'the docs'"
          ></a>
        </div>
      </div>
    </ng-template>
  `,
  styles: `
    :host {
      height: 100%;
      min-height: var(--tui-height-l);
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      gap: 0.5rem;
      margin-bottom: 1.5rem;

      [tuiCell],
      [tuiTitle],
      [tuiSubtitle] {
        margin: 0;
        justify-content: center;

        &::after {
          display: none;
        }
      }
    }

    tui-icon {
      display: none;
    }

    tui-notification {
      width: fit-content;
    }

    :host-context(tui-root._mobile) {
      tui-notification {
        display: none;
      }

      tui-icon {
        display: block;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    TuiNotification,
    TuiTitle,
    TuiLink,
    TuiCell,
    TuiIcon,
    TuiHint,
  ],
})
export class TimeComponent {
  readonly now = toSignal(inject(TimeService).now$)
}
