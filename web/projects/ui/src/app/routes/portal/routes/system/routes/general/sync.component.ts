import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification, TuiTitle } from '@taiga-ui/core'

@Component({
  selector: 'system-sync',
  template: `
    <tui-notification appearance="warning">
      <div tuiTitle>
        Clock sync failure
        <div tuiSubtitle>
          This will cause connectivity issues. Refer to the
          <a
            tuiLink
            iconEnd="@tui.external-link"
            href="https://docs.start9.com/0.3.5.x/support/common-issues#clock-sync-failure"
            target="_blank"
            rel="noreferrer"
            [textContent]="'StartOS docs'"
          ></a>
          to resolve it
        </div>
      </div>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification, TuiTitle, TuiLink],
})
export class SystemSyncComponent {}
