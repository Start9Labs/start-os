import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiLink, TuiNotification, TuiTitle } from '@taiga-ui/core'
import { i18nPipe } from 'src/app/i18n/i18n.pipe'

@Component({
  selector: 'system-sync',
  template: `
    <tui-notification appearance="warning">
      <div tuiTitle>
        {{ 'system.general.sync.title' | i18n }}
        <div tuiSubtitle>
          {{ 'system.general.sync.subtitle' | i18n }}
          <a
            tuiLink
            iconEnd="@tui.external-link"
            href="https://docs.start9.com/0.3.5.x/support/common-issues#clock-sync-failure"
            target="_blank"
            rel="noreferrer"
            [textContent]="'StartOS docs'"
          ></a>
        </div>
      </div>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification, TuiTitle, TuiLink, i18nPipe],
})
export class SystemSyncComponent {}
