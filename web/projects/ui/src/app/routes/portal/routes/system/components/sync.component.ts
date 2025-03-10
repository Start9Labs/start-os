import { TuiCell } from '@taiga-ui/layout'
import { TuiTitle, TuiButton, TuiNotification } from '@taiga-ui/core'
import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'system-sync',
  template: `
    <tui-notification appearance="warning">
      <div tuiCell [style.padding]="0">
        <div tuiTitle>
          Clock sync failure
          <div tuiSubtitle>
            This will cause connectivity issues. Refer to the StartOS docs to
            resolve the issue.
          </div>
        </div>
        <a
          tuiButton
          appearance="secondary-grayscale"
          iconEnd="@tui.external-link"
          href="https://docs.start9.com/0.3.5.x/support/common-issues#clock-sync-failure"
          target="_blank"
          rel="noreferrer"
        >
          Open Docs
        </a>
      </div>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton, TuiCell, TuiNotification, TuiTitle],
})
export class SystemSyncComponent {}
