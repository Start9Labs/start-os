import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiNotificationModule } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'

@Component({
  selector: 'settings-sync',
  template: `
    <tui-notification status="warning">
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
          appearance="glass"
          iconRight="tuiIconExternalLinkLarge"
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
  imports: [
    TuiButtonModule,
    TuiCellModule,
    TuiNotificationModule,
    TuiTitleModule,
  ],
})
export class SettingsSyncComponent {}
